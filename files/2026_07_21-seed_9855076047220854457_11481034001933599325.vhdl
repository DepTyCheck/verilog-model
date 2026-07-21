-- Seed: 9855076047220854457,11481034001933599325

entity gebhp is
  port (vsymlcuf : linkage real; dyp : in time);
end gebhp;

architecture xmzw of gebhp is
  
begin
  
end xmzw;

entity wkbwjz is
  port (p : in severity_level);
end wkbwjz;

architecture lytxybqyly of wkbwjz is
  
begin
  
end lytxybqyly;

entity gu is
  port (vfniye : linkage boolean_vector(4 downto 2); yygocz : out integer);
end gu;

architecture lwykg of gu is
  signal kjz : severity_level;
  signal ylsbm : severity_level;
  signal klez : time;
  signal wkz : real;
begin
  nwyifskrv : entity work.gebhp
    port map (vsymlcuf => wkz, dyp => klez);
  irqurc : entity work.wkbwjz
    port map (p => ylsbm);
  atesqsqip : entity work.wkbwjz
    port map (p => kjz);
  sretjx : entity work.wkbwjz
    port map (p => kjz);
  
  -- Single-driven assignments
  klez <= klez;
  ylsbm <= kjz;
  kjz <= ylsbm;
  yygocz <= 0_1;
end lwykg;



-- Seed after: 18371625586626090472,11481034001933599325
