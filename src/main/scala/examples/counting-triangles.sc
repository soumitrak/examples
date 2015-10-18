/**
 * I have taken the graph at https://www.vertica.com/2011/09/21/counting-triangles/ for testing.
 */

val edges = List (
    ("Ben", "Chuck"),
    ("Ben", "Stephen"),
    ("Chuck", "Stephen"),
    ("Chuck", "Rajat"),
    ("Rajat", "Stephen"),
    ("Andrew", "Ben"),
    ("Andrew", "Matt"),
    ("Andrew", "Pachu"),
    ("Matt", "Pachu"),
    ("Chuck", "Lyric")
)

edges.map { ab =>
    if (ab._1 < ab._2)
        ab
    else
        ab.swap
}.groupBy (_._1).toList.flatMap {
    case (center, friend_edges) =>
        val possible_triangles = friend_edges.map (_._2).sorted.combinations (2).map {
            case a :: b :: Nil =>
                (a, b, center, 0)
        }
        val real_edges = friend_edges.map (ab => (ab._1, ab._2, null, 1))
        real_edges ++ possible_triangles
}.groupBy (ab => (ab._1, ab._2)).values.foreach { list =>
    list.exists (_._4 == 1) match {
        case true =>
            // Print triangles.
            list.filter (_._3 != null).foreach {
                case (b, c, a, 0) =>
                    println (s"Triangle: $a -> $b -> $c")
            }
        case _ =>
    }
}
