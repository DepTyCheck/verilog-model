-- Seed: 12841567862225094755,14629254427735353553

entity v is
  port (uynyvzt : buffer integer_vector(0 to 2); yo : inout real);
end v;

architecture ee of v is
  
begin
  -- Single-driven assignments
  yo <= 2#00.0_0_1#;
  uynyvzt <= (8#74526#, 2#1011#, 34320);
end ee;

entity pbl is
  port (qugaocl : linkage time; jva : inout bit);
end pbl;

architecture hyp of pbl is
  signal nygfpzp : real;
  signal hc : integer_vector(0 to 2);
  signal xno : real;
  signal yskgvkdutc : integer_vector(0 to 2);
begin
  yv : entity work.v
    port map (uynyvzt => yskgvkdutc, yo => xno);
  w : entity work.v
    port map (uynyvzt => hc, yo => nygfpzp);
  
  -- Single-driven assignments
  jva <= '0';
end hyp;

library ieee;
use ieee.std_logic_1164.all;

entity pbty is
  port (o : buffer integer_vector(1 to 3); wilrtvmyhr : inout time_vector(3 downto 4); pgdvfvabj : in std_logic);
end pbty;

architecture xqqnkuxcv of pbty is
  signal leo : real;
  signal ksnv : integer_vector(0 to 2);
  signal uppr : real;
begin
  sn : entity work.v
    port map (uynyvzt => o, yo => uppr);
  ewehgg : entity work.v
    port map (uynyvzt => ksnv, yo => leo);
  
  -- Single-driven assignments
  wilrtvmyhr <= (others => 0 ns);
end xqqnkuxcv;

entity oll is
  port (facxcrrr : linkage time; wibhlmioua : in real; vxohzs : linkage character; yxfie : linkage real);
end oll;

architecture cmhhxt of oll is
  signal olzvjpddrh : real;
  signal wjqf : integer_vector(0 to 2);
  signal uif : real;
  signal h : integer_vector(0 to 2);
  signal xostktz : bit;
begin
  vpqpkwbsz : entity work.pbl
    port map (qugaocl => facxcrrr, jva => xostktz);
  jplkcpkl : entity work.v
    port map (uynyvzt => h, yo => uif);
  ewyagn : entity work.v
    port map (uynyvzt => wjqf, yo => olzvjpddrh);
end cmhhxt;



-- Seed after: 11148759080230920497,14629254427735353553
