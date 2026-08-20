-- Seed: 16694236941043079777,499459191852795575

entity fpddyfjwyp is
  port (zfyclredj : out integer_vector(0 to 3));
end fpddyfjwyp;

architecture dzp of fpddyfjwyp is
  
begin
  
end dzp;

entity olj is
  port (deksr : out integer);
end olj;

architecture pnclrg of olj is
  signal bpg : integer_vector(0 to 3);
  signal zwg : integer_vector(0 to 3);
  signal v : integer_vector(0 to 3);
begin
  alqxgdv : entity work.fpddyfjwyp
    port map (zfyclredj => v);
  luqefh : entity work.fpddyfjwyp
    port map (zfyclredj => zwg);
  apljfwe : entity work.fpddyfjwyp
    port map (zfyclredj => bpg);
  
  -- Single-driven assignments
  deksr <= deksr;
end pnclrg;

library ieee;
use ieee.std_logic_1164.all;

entity jsjkgynyg is
  port (yw : buffer character; ra : buffer std_logic_vector(1 to 3));
end jsjkgynyg;

architecture wagfnk of jsjkgynyg is
  
begin
  -- Single-driven assignments
  yw <= 'e';
  
  -- Multi-driven assignments
  ra <= "UWL";
  ra <= "LLH";
  ra <= "H-0";
  ra <= ra;
end wagfnk;

library ieee;
use ieee.std_logic_1164.all;

entity sd is
  port (vkio : out real_vector(0 to 2); om : out time; uobkiyf : out std_logic);
end sd;

library ieee;
use ieee.std_logic_1164.all;

architecture c of sd is
  signal bnkfiye : std_logic_vector(1 to 3);
  signal x : character;
  signal txim : integer;
begin
  aqzpjelyc : entity work.olj
    port map (deksr => txim);
  jalqi : entity work.jsjkgynyg
    port map (yw => x, ra => bnkfiye);
  
  -- Multi-driven assignments
  bnkfiye <= bnkfiye;
  bnkfiye <= bnkfiye;
  uobkiyf <= uobkiyf;
end c;



-- Seed after: 1037083677441736757,499459191852795575
