-- Seed: 1534328471852096916,7198033922882419595

entity ehbn is
  port (mh : in integer);
end ehbn;

architecture xfb of ehbn is
  
begin
  
end xfb;

entity fr is
  port (rhhzyh : in boolean);
end fr;

architecture t of fr is
  signal msspyaj : integer;
begin
  acvmfo : entity work.ehbn
    port map (mh => msspyaj);
  jwhayvt : entity work.ehbn
    port map (mh => msspyaj);
  
  -- Single-driven assignments
  msspyaj <= 2#0_1_1_1#;
end t;



-- Seed after: 8574242888585782156,7198033922882419595
