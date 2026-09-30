-- Seed: 1705315232532113186,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity ywvxint is
  port (suycxps : linkage std_logic_vector(3 downto 3); puvlwvv : buffer integer_vector(1 to 0));
end ywvxint;

architecture vdha of ywvxint is
  
begin
  -- Single-driven assignments
  puvlwvv <= (others => 0);
end vdha;

entity pkoy is
  port (vgdhxpob : linkage real; dlvzggy : linkage boolean_vector(0 to 4); i : buffer boolean_vector(4 to 0));
end pkoy;

library ieee;
use ieee.std_logic_1164.all;

architecture bhnu of pkoy is
  signal xczxmgy : integer_vector(1 to 0);
  signal okcgoyligy : std_logic_vector(3 downto 3);
  signal iaz : integer_vector(1 to 0);
  signal sqwljmxzfo : integer_vector(1 to 0);
  signal hedhhinfc : std_logic_vector(3 downto 3);
  signal actlevmq : integer_vector(1 to 0);
  signal hf : std_logic_vector(3 downto 3);
begin
  czipc : entity work.ywvxint
    port map (suycxps => hf, puvlwvv => actlevmq);
  chqwbcryu : entity work.ywvxint
    port map (suycxps => hedhhinfc, puvlwvv => sqwljmxzfo);
  kvihyjbim : entity work.ywvxint
    port map (suycxps => hedhhinfc, puvlwvv => iaz);
  kwvwpzepea : entity work.ywvxint
    port map (suycxps => okcgoyligy, puvlwvv => xczxmgy);
  
  -- Single-driven assignments
  i <= i;
  
  -- Multi-driven assignments
  hedhhinfc <= (others => 'W');
  hedhhinfc <= (others => '0');
  hf <= "-";
  hf <= "-";
end bhnu;

library ieee;
use ieee.std_logic_1164.all;

entity mit is
  port (h : in integer; vrnqbeaah : in real; vatu : buffer integer; ljbeq : inout std_logic_vector(4 downto 0));
end mit;

architecture ygzt of mit is
  signal qlzxtekrd : boolean_vector(4 to 0);
  signal wgp : boolean_vector(0 to 4);
  signal sbmfpoa : real;
begin
  cmypw : entity work.pkoy
    port map (vgdhxpob => sbmfpoa, dlvzggy => wgp, i => qlzxtekrd);
  
  -- Single-driven assignments
  vatu <= vatu;
  
  -- Multi-driven assignments
  ljbeq <= ljbeq;
  ljbeq <= "XLW0W";
  ljbeq <= "WU10-";
  ljbeq <= "UXW-Z";
end ygzt;



-- Seed after: 16023088051516232754,12260394286515585877
