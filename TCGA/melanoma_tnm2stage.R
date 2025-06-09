tnm_to_stage <- function(tnm_vector) {
  stage <- rep("Unstageable", length(tnm_vector))

  # Extract T, N, M with defaulting for missing components
  t <- ifelse(grepl("T[0-4a-bisxX]+", tnm_vector),
              toupper(sub("(.*)?(T[0-4a-bisxX]+).*", "\\2", tnm_vector)),
              "TX")
  n <- ifelse(grepl("N[0-3a-cxX]+", tnm_vector),
              toupper(sub(".*(N[0-3a-cxX]+).*", "\\1", tnm_vector)),
              "NX")
  m <- ifelse(grepl("M[0-1a-dc]+", tnm_vector),
              toupper(sub(".*(M[0-1a-dc]+)", "\\1", tnm_vector)),
              "MX")

  # -------------------------
  # Specific stages (M0 assumed)
  stage[t == "TIS" & n == "N0" & m == "M0"] <- "Stage 0"
  stage[t %in% c("T1A", "T1B", "T2A") & n == "N0" & m == "M0"] <- "Stage I"
  stage[t %in% c("T2B", "T3A", "T3B", "T4A", "T4B") & n == "N0" & m == "M0"] <- "Stage II"
  stage[grepl("^N[1-3]", n) & m == "M0"] <- "Stage III"
  stage[grepl("^M1", m)] <- "Stage IV"

  # -------------------------
  # Assumed or broad-stage logic
  stage[t %in% c("T1A", "T1B", "T2A") & n == "NX" & m == "M0"] <- "Stage I (assumed)"
  stage[t %in% c("T3A", "T3B", "T4A", "T4B") & n == "NX" & m == "M0"] <- "Stage II (assumed)"
  stage[grepl("^N[1-3]", n) & m == "MX"] <- "Stage III (assumed)"
  stage[t == "TX" & n == "N0" & m == "M0"] <- "Stage I–II (T unknown)"
  stage[t == "TX" & n == "NX" & m == "M0"] <- "Stage I–III (T/N unknown)"
  stage[t %in% c("T4", "T4A", "T4B") & n == "NX" & m == "M0"] <- "Stage II–III (N unknown)"

  return(stage)
}



