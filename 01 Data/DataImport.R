require(data.world)

elections = query(
  data.world(),
  dataset="vcjaladi/s-17-dv-final-project", type="sql",
  query=
    "SELECT *
    FROM finalproject_ElectionsData
    LIMIT 5")

View(elections)