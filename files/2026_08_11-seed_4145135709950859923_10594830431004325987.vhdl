-- Seed: 4145135709950859923,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity pxocy is
  port (n : in std_logic_vector(3 downto 1); extvilnk : buffer bit_vector(1 to 1); sjhcjypbls : buffer real; qbe : in real_vector(1 to 1));
end pxocy;

architecture fpmhzuzg of pxocy is
  
begin
  
end fpmhzuzg;

library ieee;
use ieee.std_logic_1164.all;

entity dxw is
  port (bvhen : inout bit; bmxpab : in time_vector(0 downto 4); kufiaokj : out std_logic_vector(2 to 2); ioo : out std_logic_vector(3 to 4));
end dxw;

library ieee;
use ieee.std_logic_1164.all;

architecture zduzmukpg of dxw is
  signal jukzmyk : real_vector(1 to 1);
  signal neaudospdy : real;
  signal banlg : bit_vector(1 to 1);
  signal zozqow : std_logic_vector(3 downto 1);
  signal pirvw : real;
  signal hmys : bit_vector(1 to 1);
  signal rl : real_vector(1 to 1);
  signal tq : real;
  signal fvoydri : bit_vector(1 to 1);
  signal njpvv : std_logic_vector(3 downto 1);
begin
  ryzq : entity work.pxocy
    port map (n => njpvv, extvilnk => fvoydri, sjhcjypbls => tq, qbe => rl);
  o : entity work.pxocy
    port map (n => njpvv, extvilnk => hmys, sjhcjypbls => pirvw, qbe => rl);
  hpivau : entity work.pxocy
    port map (n => zozqow, extvilnk => banlg, sjhcjypbls => neaudospdy, qbe => jukzmyk);
  
  -- Single-driven assignments
  bvhen <= bvhen;
  jukzmyk <= rl;
  
  -- Multi-driven assignments
  ioo <= ('Z', 'L');
  ioo <= ioo;
end zduzmukpg;



-- Seed after: 13568106481047420681,10594830431004325987
