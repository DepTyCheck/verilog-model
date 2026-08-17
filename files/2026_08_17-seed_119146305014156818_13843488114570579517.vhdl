-- Seed: 119146305014156818,13843488114570579517

entity gerlum is
  port (jqpbvi : inout integer; cknalrmg : inout real; faposi : in integer; agmn : out integer);
end gerlum;

architecture vdefe of gerlum is
  
begin
  -- Single-driven assignments
  agmn <= 2_4_0;
end vdefe;

library ieee;
use ieee.std_logic_1164.all;

entity sisv is
  port (osovdtwlvg : linkage integer_vector(0 to 2); ng : buffer std_logic; evtebqt : out std_logic_vector(4 downto 3));
end sisv;

architecture zm of sisv is
  signal n : integer;
  signal qkhwjf : real;
  signal cajzprxbce : integer;
  signal adunnekci : integer;
  signal r : real;
  signal wpfywcp : integer;
begin
  kx : entity work.gerlum
    port map (jqpbvi => wpfywcp, cknalrmg => r, faposi => adunnekci, agmn => adunnekci);
  fwu : entity work.gerlum
    port map (jqpbvi => cajzprxbce, cknalrmg => qkhwjf, faposi => wpfywcp, agmn => n);
end zm;

library ieee;
use ieee.std_logic_1164.all;

entity udzgtozzu is
  port (qxvcle : linkage real; o : buffer std_logic_vector(2 to 4); fttrtqstg : in real; ki : inout std_logic_vector(1 downto 2));
end udzgtozzu;

library ieee;
use ieee.std_logic_1164.all;

architecture ndulojpiv of udzgtozzu is
  signal j : integer;
  signal s : integer;
  signal jimvmsr : real;
  signal fclxazbzq : std_logic_vector(4 downto 3);
  signal gxozw : std_logic;
  signal aowiz : integer_vector(0 to 2);
  signal qcnmytoxk : integer;
  signal yaldjjbp : integer;
  signal joep : real;
  signal zeg : integer;
begin
  cugtogq : entity work.gerlum
    port map (jqpbvi => zeg, cknalrmg => joep, faposi => yaldjjbp, agmn => qcnmytoxk);
  jbfuzfuwnx : entity work.sisv
    port map (osovdtwlvg => aowiz, ng => gxozw, evtebqt => fclxazbzq);
  rvgooh : entity work.gerlum
    port map (jqpbvi => yaldjjbp, cknalrmg => jimvmsr, faposi => s, agmn => j);
  
  -- Single-driven assignments
  s <= zeg;
  
  -- Multi-driven assignments
  ki <= (others => '0');
end ndulojpiv;



-- Seed after: 2237311018399743712,13843488114570579517
