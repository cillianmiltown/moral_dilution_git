
x <- df_complete
x <- x[which(x$valence=="bad"),]

  x$dv <- x$M1


  z <- 2.0452#1.959964

  df_1 <- x[which(x$condition=="diagnostic"),]
  df_2 <- x[which(x$condition=="non-diagnostic"),]


  df_1 <- df_1[order(df_1$ResponseId), ]
  df_2 <- df_2[order(df_2$ResponseId), ]

  x1 <- mean(df_1$dv)
  x2 <- mean(df_2$dv)

  n1 <- length(df_1$gender)
  n2 <- length(df_2$gender)

  s1 <- sd(df_1$dv)
  s2 <- sd(df_2$dv)

  s_pooled <- sqrt(
    (
      ((s1*s1)*(n1-1))+((s2*s2)*(n2-1))
    )/
      (n1+n1-2)
  )

  ci_u <- (x1-x2) - (z*(
    sqrt(
      ((s1*s1)/n1)+
        ((s2*s2)/n2)
      )
    )
  )

  ci_u

  d <- (x1-x2)/s_pooled


  d_paired <- (
    mean(df_1$dv-df_2$dv)/
      sd(df_1$dv-df_2$dv)
  )

  d <- d_paired
  diff <- mean(df_1$dv-df_2$dv)

  vd <- ((n1+n2)/(n1*n2))+((d*d)/(2*(n1+n2)))

  sed <- sqrt(vd)

  ci_upper <- diff+(z*sed)
  ci_lower <- diff-(z*sed)

  d_paired <- (
    mean(df_1$dv-df_2$dv)/
      sd(df_1$dv-df_2$dv)
  )


  n <- length(x$gender)/4
  var <- (1/n)+((d_paired*d_paired)/(2*n))
  vd <- var
  sed <- sqrt(vd)


  ci_upper <- d_paired+(z*sed)
  ci_lower <- d_paired-(z*sed)

  list(study="Study 3",
       x1=x1
       ,x2=x2
       ,s1=s1
       ,s2=s2
       ,n1=n1
       ,n2=n2
       ,s_pooled=s_pooled
       ,d=d
       ,vd=vd
       ,sed=sed
       ,ci_upper=ci_upper
       ,ci_lower=ci_lower
       ,p=p2
       ,d_paired=d_paired)

