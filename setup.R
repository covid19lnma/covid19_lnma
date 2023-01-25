drugs_or_blood="drugs"
placebo="standard care/placebo"
folderROM = drugs_or_blood

if (!dir.exists("NMA/drugs")){ # create directories
  dir.create("NMA/drugs")
}

if (!dir.exists("NMA/blood")){ # create directories
  dir.create("NMA/blood")
}

if (!dir.exists("NMA/prohylaxis")){ # create directories
  dir.create("NMA/prohylaxis")
}


if (!dir.exists("pairwise/drugs")){ # create directories
  dir.create("pairwise/drugs")
}

if (!dir.exists("pairwise/blood")){ # create directories
  dir.create("pairwise/blood")
}

if (!dir.exists("pairwise/prohylaxis")){ # create directories
  dir.create("pairwise/prohylaxis")
}

if (!dir.exists("input/")){ # create directories
  dir.create("input/")
}
