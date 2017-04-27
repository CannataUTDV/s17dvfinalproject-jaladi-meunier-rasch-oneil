require(data.world)

elections = query(
  data.world(),
  dataset="vcjaladi/s-17-dv-project-6", type="sql",
  query=
    "SELECT *
    FROM CountyElections")

View(elections)