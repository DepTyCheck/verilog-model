-- Seed: 3430288674877144600,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity uadgoxrjm is
  port (itktsu : inout std_logic_vector(1 to 2); oe : buffer integer; ltmgi : buffer std_logic);
end uadgoxrjm;

architecture vxym of uadgoxrjm is
  
begin
  -- Single-driven assignments
  oe <= 2;
  
  -- Multi-driven assignments
  ltmgi <= 'Z';
  itktsu <= ('1', 'U');
  ltmgi <= '-';
end vxym;

library ieee;
use ieee.std_logic_1164.all;

entity nzo is
  port (filwxl : linkage integer; ojr : inout std_logic);
end nzo;

library ieee;
use ieee.std_logic_1164.all;

architecture cjnfw of nzo is
  signal p : integer;
  signal i : std_logic;
  signal jkuvslmn : integer;
  signal nhlvwcxtzp : std_logic_vector(1 to 2);
  signal snfpeejza : std_logic;
  signal slzv : integer;
  signal brdnrzt : std_logic_vector(1 to 2);
  signal caetobeqp : integer;
  signal lqdccf : std_logic_vector(1 to 2);
begin
  sduthw : entity work.uadgoxrjm
    port map (itktsu => lqdccf, oe => caetobeqp, ltmgi => ojr);
  xmfzmotz : entity work.uadgoxrjm
    port map (itktsu => brdnrzt, oe => slzv, ltmgi => snfpeejza);
  r : entity work.uadgoxrjm
    port map (itktsu => nhlvwcxtzp, oe => jkuvslmn, ltmgi => i);
  z : entity work.uadgoxrjm
    port map (itktsu => lqdccf, oe => p, ltmgi => ojr);
  
  -- Multi-driven assignments
  nhlvwcxtzp <= "0Z";
  i <= 'X';
  ojr <= 'X';
  snfpeejza <= '1';
end cjnfw;



-- Seed after: 16222450145245873621,6882842853887419669
