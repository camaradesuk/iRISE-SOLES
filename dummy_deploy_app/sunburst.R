format_as_sunburst <- function(data, col1, col2, col3){
  
  level1 <- rlang::enquo(col1)
  level2 <- rlang::enquo(col2)
  level3 <- rlang::enquo(col3)
  
  testdata <- data %>%
    tidyr::unnest() %>%
    select(uid, !!level1, !!level2, !!level3) %>%
    unique() %>%
    tidyr::drop_na() %>%
    group_by_all() %>%
    count() %>%
    rename(value = n) %>%
    ungroup()
  
  DF0 <- testdata %>%
    group_by(!!level1) %>%
    unique() %>%
    summarise(value=sum(value))%>%
    ungroup()
  
  DF1 <- testdata %>%
    group_by(!!level1, !!level2) %>%
    summarise(value=sum(value))%>%
    ungroup()
  
  DF2 <- testdata %>%
    group_by(!!level1, !!level2, !!level3) %>%
    summarise(value=sum(value)) %>%
    ungroup()
  
  df0 <- DF0 %>%
    rename(ids = !!level1) %>%
    mutate(labels = ids) %>%
    mutate(parents = "")
  
  df0 <- as.data.frame(df0)
  
  df1 <- DF1 %>%
    mutate(ids = paste(!!level1, "-", !!level2),
           labels = paste0(!!level2),
           parents = paste0(!!level1)) %>%
    select(ids, labels, parents, value)
  
  df1 <- as.data.frame(df1)
  
  df2 <- DF2 %>%
    mutate(ids = paste(!!level1, "-", !!level2, "-",  !!level3),
           labels =paste(!!level3),
           parents = paste(!!level1, "-", !!level2)) %>%
    select(ids, labels, parents, value)
  
  df2 <- as.data.frame(df2)
  
  df <- rbind(df0, df1, df2)
  
  
  return(df)
  
}

format_as_sunburst_2 <- function(data, col1, col2){
  
  level1 <- rlang::enquo(col1)
  level2 <- rlang::enquo(col2)
  
  testdata <- data %>%
    tidyr::unnest() %>%
    select(uid, !!level1, !!level2) %>%
    unique() %>%
    tidyr::drop_na() %>%
    group_by_all() %>%
    count() %>%
    rename(value = n) %>%
    ungroup()
  
  DF0 <- testdata %>%
    group_by(!!level1) %>%
    unique() %>%
    summarise(value=sum(value))%>%
    ungroup()
  
  DF1 <- testdata %>%
    group_by(!!level1, !!level2) %>%
    summarise(value=sum(value))%>%
    ungroup()
  
  
  df0 <- DF0 %>%
    rename(ids = !!level1) %>%
    mutate(labels = ids) %>%
    mutate(parents = "")
  
  df0 <- as.data.frame(df0)
  
  df1 <- DF1 %>%
    mutate(ids = paste(!!level1, "-", !!level2),
           labels = paste0(!!level2),
           parents = paste0(!!level1)) %>%
    select(ids, labels, parents, value)
  
  
  df1 <- as.data.frame(df1)
  
  df <- rbind(df0, df1)
  
  
  return(df)
  
}

