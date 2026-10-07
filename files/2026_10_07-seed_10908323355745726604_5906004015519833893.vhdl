-- Seed: 10908323355745726604,5906004015519833893

entity znqyt is
  port (xw : out boolean_vector(0 downto 3); isdb : linkage real; ew : in real);
end znqyt;

architecture fzqvf of znqyt is
  
begin
  -- Single-driven assignments
  xw <= (others => TRUE);
end fzqvf;

library ieee;
use ieee.std_logic_1164.all;

entity pkqunwva is
  port (dylen : in std_logic);
end pkqunwva;

architecture gthk of pkqunwva is
  signal jr : real;
  signal xhqr : real;
  signal bvant : boolean_vector(0 downto 3);
  signal bsyb : real;
  signal n : real;
  signal ldx : boolean_vector(0 downto 3);
  signal shkqprlz : real;
  signal kyahteyfbq : boolean_vector(0 downto 3);
  signal idcw : real;
  signal rogtqsq : real;
  signal vbtk : boolean_vector(0 downto 3);
begin
  xkt : entity work.znqyt
    port map (xw => vbtk, isdb => rogtqsq, ew => idcw);
  ntku : entity work.znqyt
    port map (xw => kyahteyfbq, isdb => shkqprlz, ew => rogtqsq);
  mu : entity work.znqyt
    port map (xw => ldx, isdb => n, ew => bsyb);
  yysvdlb : entity work.znqyt
    port map (xw => bvant, isdb => xhqr, ew => jr);
end gthk;



-- Seed after: 3633993862531338685,5906004015519833893
