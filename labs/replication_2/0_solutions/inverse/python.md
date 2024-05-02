
# Python


``` python
import numpy as np

np.random.seed(3)
A = np.random.randn(10, 9)
A = np.hstack([A, (2 * A[:, 0] + 3 * A[:, 1] - A[:, 8]).reshape(10, 1)])

inv_A = np.linalg.inv(A)
```

    LinAlgError: Singular matrix
