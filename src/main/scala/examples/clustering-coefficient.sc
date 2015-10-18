/**
 * Calculate Clustering coefficient (https://en.wikipedia.org/wiki/Clustering_coefficient)
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

// Get list of friends for every persons.
val friends_per_vertex = edges.flatMap (ab =>
    ab :: ab.swap :: Nil
).groupBy (_._1)

// Get the list of possible triangles per person, and list of real connections.
val possible_triangles_per_vertex = friends_per_vertex.toList.flatMap {
    case (center, friend_edges) =>
        // Possible triangles for every person.
        val friends = friend_edges.map (_._2).sorted.combinations (2).map {
            case a :: b :: Nil =>
                (a, b, center, 0)
        }

        // Connections in the graph.
        val real_edges = friend_edges.flatMap { ab =>
            if (ab._1 < ab._2) Some (ab._1, ab._2, null, 1) else None
        }

        real_edges ++ friends
}

possible_triangles_per_vertex.groupBy (ab => (ab._1, ab._2))
    .values.flatMap { abcd =>
    val connected = if (abcd.exists (_._4 == 1)) 1 else 0
    val neighbours = abcd.flatMap {
        case (_, _, null, _) => None
        case (_, _, center, 0) => Some ((center, connected))
    }
    neighbours
}.groupBy (_._1).foreach { ab =>
    val total = ab._2.size
    val connected = ab._2.count (_._2 == 1)
    println (s"${ab._1} -> $connected / $total")
}
