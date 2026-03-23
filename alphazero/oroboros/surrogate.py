"""Surrogate model: cheap prediction of game metrics from ruleset parameters.

Encodes discrete RuleSet parameters as a fixed-length feature vector and
trains a Random Forest to predict objective values.  Used to pre-screen
candidate rulesets before expensive simulation/AlphaZero evaluation.
"""

from __future__ import annotations
import numpy as np
from dataclasses import dataclass, field

from .ruleset import (
    RuleSet,
    BOARD_SYMMETRIES, NUM_RINGS, NUM_TYPES, ELEMENTS_PER_PLAYER,
    FOOD_INITIAL, WIN_TYPES, WIN_THRESHOLDS, NUM_PLAYERS, CONFLICT_VALUES,
)

# Maximum conflict pairs (num_types=4 → 6 pairs)
_MAX_CONFLICT_PAIRS = 6


class RuleSetEncoder:
    """Encode a RuleSet into a fixed-length numeric vector.

    Layout (58 features):
      board_symmetry    one-hot [3,4,5,6]     →  4
      num_rings         one-hot [3,4,5,6]     →  4
      num_types         one-hot [2,3,4]       →  3
      elements_per_player one-hot [2,3,4,5]   →  4
      food_enabled      binary                →  1
      food_initial      one-hot [0,1,2]       →  3
      can_move..circulate binary each          →  5
      conflict          one-hot per pair × 4   → 24 (padded to 6 pairs)
      win_type          one-hot [cap,pop]     →  2
      win_threshold     one-hot [3,5,7,10]    →  4
      num_players       one-hot [2,3,4,5]     →  4
                                        Total: 58
    """

    def __init__(self):
        self._onehot_specs: list[tuple[str, list]] = [
            ("board_symmetry",      BOARD_SYMMETRIES),
            ("num_rings",           NUM_RINGS),
            ("num_types",           NUM_TYPES),
            ("elements_per_player", ELEMENTS_PER_PLAYER),
            ("food_initial",        FOOD_INITIAL),
            ("win_type",            WIN_TYPES),
            ("win_threshold",       WIN_THRESHOLDS),
            ("num_players",         NUM_PLAYERS),
        ]
        self._binary_fields = [
            "food_enabled", "can_move", "can_eat", "can_grow",
            "can_capture", "can_circulate",
        ]
        # Pre-compute total size
        oh_size = sum(len(domain) for _, domain in self._onehot_specs)
        bin_size = len(self._binary_fields)
        conflict_size = _MAX_CONFLICT_PAIRS * len(CONFLICT_VALUES)
        self.n_features = oh_size + bin_size + conflict_size

    def encode(self, rs: RuleSet) -> np.ndarray:
        parts: list[np.ndarray] = []

        # One-hot categorical fields
        for fname, domain in self._onehot_specs:
            vec = np.zeros(len(domain), dtype=np.float32)
            val = getattr(rs, fname)
            if val in domain:
                vec[domain.index(val)] = 1.0
            parts.append(vec)

        # Binary fields
        for fname in self._binary_fields:
            parts.append(np.array([float(getattr(rs, fname))], dtype=np.float32))

        # Conflict tuple — one-hot per pair, padded
        conflict_vec = np.zeros(_MAX_CONFLICT_PAIRS * len(CONFLICT_VALUES), dtype=np.float32)
        for i, cv in enumerate(rs.conflict):
            if i < _MAX_CONFLICT_PAIRS and cv in CONFLICT_VALUES:
                offset = i * len(CONFLICT_VALUES) + CONFLICT_VALUES.index(cv)
                conflict_vec[offset] = 1.0
        parts.append(conflict_vec)

        return np.concatenate(parts)

    def feature_names(self) -> list[str]:
        names: list[str] = []
        for fname, domain in self._onehot_specs:
            for v in domain:
                names.append(f"{fname}={v}")
        for fname in self._binary_fields:
            names.append(fname)
        for i in range(_MAX_CONFLICT_PAIRS):
            for cv in CONFLICT_VALUES:
                names.append(f"conflict_{i}={cv}")
        return names


# ── Objective keys (must match SearchResult.objectives()) ─────────────────────

OBJECTIVE_KEYS = ["depth", "complexity", "balance", "interaction", "mechanism", "simplicity"]


class SurrogateModel:
    """Lightweight predictor: RuleSet → predicted objectives.

    Uses scikit-learn ExtraTreesRegressor (fast, handles small datasets,
    no hyperparameter tuning needed).
    """

    def __init__(
        self,
        target_keys: list[str] | None = None,
        min_samples: int = 15,
    ):
        self.target_keys = target_keys or OBJECTIVE_KEYS
        self.encoder = RuleSetEncoder()
        self.min_samples = min_samples

        self._X: list[np.ndarray] = []
        self._Y: list[np.ndarray] = []
        self._model = None
        self.is_fitted = False

    def add_observation(self, rs: RuleSet, objectives: dict[str, float]):
        x = self.encoder.encode(rs)
        y = np.array([objectives.get(k, 0.0) for k in self.target_keys], dtype=np.float32)
        self._X.append(x)
        self._Y.append(y)
        self.is_fitted = False  # invalidate

    def fit(self):
        if len(self._X) < self.min_samples:
            self.is_fitted = False
            return

        X = np.stack(self._X)
        Y = np.stack(self._Y)

        # Try sklearn first (better), fall back to KNN
        try:
            from sklearn.ensemble import ExtraTreesRegressor
            self._model = ExtraTreesRegressor(
                n_estimators=50, max_depth=8, random_state=42, n_jobs=1,
            )
            self._model.fit(X, Y)
            self._backend = "sklearn"
        except ImportError:
            # Pure-numpy KNN fallback
            self._X_train = X
            self._Y_train = Y
            self._model = "knn"
            self._backend = "knn"

        self.is_fitted = True

    def _knn_predict(self, X_query: np.ndarray, k: int = 5) -> np.ndarray:
        """K-nearest-neighbors regression using stored training data."""
        # Euclidean distance in one-hot space = Hamming-like distance
        dists = np.linalg.norm(self._X_train[None, :, :] - X_query[:, None, :], axis=2)
        k = min(k, len(self._X_train))
        results = []
        for i in range(len(X_query)):
            idx = np.argpartition(dists[i], k)[:k]
            # Distance-weighted average (closer = higher weight)
            d = dists[i][idx] + 1e-8
            weights = 1.0 / d
            weights /= weights.sum()
            pred = (self._Y_train[idx] * weights[:, None]).sum(axis=0)
            results.append(pred)
        return np.array(results)

    def predict(self, rs: RuleSet) -> dict[str, float]:
        if not self.is_fitted:
            return {k: 0.0 for k in self.target_keys}
        x = self.encoder.encode(rs).reshape(1, -1)
        if self._backend == "knn":
            y = self._knn_predict(x)[0]
        else:
            y = self._model.predict(x)[0]
        return {k: float(v) for k, v in zip(self.target_keys, y)}

    def predict_batch(self, rulesets: list[RuleSet]) -> list[dict[str, float]]:
        if not self.is_fitted:
            return [{k: 0.0 for k in self.target_keys} for _ in rulesets]
        X = np.stack([self.encoder.encode(rs) for rs in rulesets])
        if self._backend == "knn":
            Y = self._knn_predict(X)
        else:
            Y = self._model.predict(X)
        return [{k: float(v) for k, v in zip(self.target_keys, row)} for row in Y]

    def score_richness(self, rs: RuleSet, weights: dict[str, float]) -> float:
        """Predict richness using the surrogate + given weights."""
        pred = self.predict(rs)
        return sum(pred.get(k, 0.0) * weights.get(k, 0.0) for k in self.target_keys)

    @property
    def n_observations(self) -> int:
        return len(self._X)

    def feature_importances(self) -> dict[str, float] | None:
        if not self.is_fitted:
            return None
        if self._backend == "sklearn":
            names = self.encoder.feature_names()
            imps = self._model.feature_importances_
            return {n: float(v) for n, v in sorted(zip(names, imps), key=lambda x: -x[1])}
        return None  # KNN doesn't have feature importances
