# addColorTable() works

    Code
      head(as.data.frame(r1), 4)
    Output
          class
      127    10
      128    10
      129    10
      130    10

---

    Code
      head(as.data.frame(r2), 4)
    Output
              class
      127 (506,547]
      128 (506,547]
      129 (506,547]
      130 (506,547]

---

    Code
      head(terra::coltab(r2)[[1]], 3)
    Output
        value red green blue alpha
      1     0 255   255  255   255
      2     1 242   242  242   255
      3     2 240   201  192   255

