-- Seed: 545060814803914258,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (wsgkgpb : in std_logic_vector(0 to 3); wc : in std_logic; yhswcvf : out real; wyvhpxmi : in boolean_vector(0 downto 4));
end u;

architecture qnwsf of u is
  
begin
  
end qnwsf;

entity vm is
  port (f : linkage severity_level; trrxobc : buffer real);
end vm;

architecture irek of vm is
  
begin
  -- Single-driven assignments
  trrxobc <= 433.1243;
end irek;

entity cfgzhiwo is
  port (lntnqdxhn : out integer_vector(4 downto 4); zeggb : out real; dpdddqaa : linkage boolean_vector(2 downto 2));
end cfgzhiwo;

library ieee;
use ieee.std_logic_1164.all;

architecture dauuru of cfgzhiwo is
  signal cfyw : real;
  signal xaszg : severity_level;
  signal lfzcbr : boolean_vector(0 downto 4);
  signal fwrctxnbb : real;
  signal rid : std_logic;
  signal rzkujguqz : std_logic_vector(0 to 3);
begin
  hbkrn : entity work.u
    port map (wsgkgpb => rzkujguqz, wc => rid, yhswcvf => fwrctxnbb, wyvhpxmi => lfzcbr);
  shinvexe : entity work.u
    port map (wsgkgpb => rzkujguqz, wc => rid, yhswcvf => zeggb, wyvhpxmi => lfzcbr);
  fjmixhecy : entity work.vm
    port map (f => xaszg, trrxobc => cfyw);
  
  -- Single-driven assignments
  lntnqdxhn <= (others => 2#1#);
  
  -- Multi-driven assignments
  rzkujguqz <= ('Z', '1', 'X', 'W');
  rzkujguqz <= ('L', 'L', 'H', 'X');
end dauuru;



-- Seed after: 5118748626246826381,3108530264173481209
