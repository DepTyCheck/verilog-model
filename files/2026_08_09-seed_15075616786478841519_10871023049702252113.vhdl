-- Seed: 15075616786478841519,10871023049702252113

entity shufanmva is
  port (fev : out time; gd : out integer);
end shufanmva;

architecture tasn of shufanmva is
  
begin
  -- Single-driven assignments
  gd <= 8#5_6_5_0#;
  fev <= 43 ms;
end tasn;



-- Seed after: 7410511512271557494,10871023049702252113
