-- Seed: 14191146642909898186,10463297573877745897

entity cight is
  port (pput : in severity_level; dscalj : in time; jvgk : out time);
end cight;

architecture cyog of cight is
  
begin
  -- Single-driven assignments
  jvgk <= 16#A_8_0_A# fs;
end cyog;

entity duii is
  port (dkajaccmg : linkage integer_vector(2 downto 0); nrlezyn : in time);
end duii;

architecture jihvqloiw of duii is
  signal cdvlpqw : time;
  signal reboabj : time;
  signal xlrsu : time;
  signal rmjc : time;
  signal zneucnpzu : time;
  signal rmpzmhzpcf : severity_level;
begin
  prthvlhh : entity work.cight
    port map (pput => rmpzmhzpcf, dscalj => zneucnpzu, jvgk => rmjc);
  ros : entity work.cight
    port map (pput => rmpzmhzpcf, dscalj => xlrsu, jvgk => reboabj);
  fizllxqnjd : entity work.cight
    port map (pput => rmpzmhzpcf, dscalj => reboabj, jvgk => cdvlpqw);
  penmepbci : entity work.cight
    port map (pput => rmpzmhzpcf, dscalj => reboabj, jvgk => zneucnpzu);
  
  -- Single-driven assignments
  rmpzmhzpcf <= WARNING;
end jihvqloiw;



-- Seed after: 8391116591910944681,10463297573877745897
