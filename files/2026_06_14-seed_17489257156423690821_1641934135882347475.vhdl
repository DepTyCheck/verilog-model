-- Seed: 17489257156423690821,1641934135882347475

library ieee;
use ieee.std_logic_1164.all;

entity xysw is
  port (njhifw : in std_logic_vector(0 downto 1); zd : linkage integer; nrfc : linkage bit);
end xysw;



architecture mkgj of xysw is
  
begin
  
end mkgj;



entity cijg is
  port (r : inout real);
end cijg;

library ieee;
use ieee.std_logic_1164.all;

architecture tythftq of cijg is
  signal opiasfisz : integer;
  signal zldhjcseos : std_logic_vector(0 downto 1);
  signal qvkj : bit;
  signal lbulhz : integer;
  signal rb : std_logic_vector(0 downto 1);
  signal qumh : std_logic_vector(0 downto 1);
  signal alugg : bit;
  signal ltbvk : integer;
  signal uh : std_logic_vector(0 downto 1);
begin
  egtdeh : entity work.xysw
    port map (njhifw => uh, zd => ltbvk, nrfc => alugg);
  xn : entity work.xysw
    port map (njhifw => qumh, zd => ltbvk, nrfc => alugg);
  zenxdiud : entity work.xysw
    port map (njhifw => rb, zd => lbulhz, nrfc => qvkj);
  dtjbk : entity work.xysw
    port map (njhifw => zldhjcseos, zd => opiasfisz, nrfc => alugg);
end tythftq;

library ieee;
use ieee.std_logic_1164.all;

entity bdlye is
  port (pqk : buffer std_logic; gylwa : in time; onmqc : in std_logic_vector(3 downto 1));
end bdlye;



architecture vrkoi of bdlye is
  signal sacycls : real;
begin
  xukcytwnei : entity work.cijg
    port map (r => sacycls);
end vrkoi;



-- Seed after: 10834801126604878450,1641934135882347475
