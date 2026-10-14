-- Seed: 13617746555727811481,2287986438313874379

entity umd is
  port (htbstslji : buffer time; gniyxgyshe : inout bit; vitfsqt : inout bit_vector(0 downto 3));
end umd;

architecture swk of umd is
  
begin
  
end swk;

library ieee;
use ieee.std_logic_1164.all;

entity vdcoa is
  port (tncdu : buffer bit_vector(4 downto 2); qe : inout std_logic_vector(0 to 1));
end vdcoa;

architecture gucoaz of vdcoa is
  signal wja : bit_vector(0 downto 3);
  signal bxk : bit;
  signal jwyjjxlfzy : time;
  signal uwtzzbb : bit_vector(0 downto 3);
  signal xgwm : bit;
  signal eovsyo : time;
begin
  ij : entity work.umd
    port map (htbstslji => eovsyo, gniyxgyshe => xgwm, vitfsqt => uwtzzbb);
  jofkdekmw : entity work.umd
    port map (htbstslji => jwyjjxlfzy, gniyxgyshe => bxk, vitfsqt => wja);
  
  -- Single-driven assignments
  tncdu <= tncdu;
  
  -- Multi-driven assignments
  qe <= ('1', 'L');
  qe <= qe;
  qe <= ('-', 'L');
end gucoaz;



-- Seed after: 10678822086119318127,2287986438313874379
