-- Seed: 3969441103314780109,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity dtgogq is
  port (lpucwyf : inout real; vrexo : buffer std_logic; eabysz : inout boolean);
end dtgogq;

architecture zrhvweyrwv of dtgogq is
  
begin
  -- Multi-driven assignments
  vrexo <= '1';
  vrexo <= 'X';
  vrexo <= 'Z';
  vrexo <= '0';
end zrhvweyrwv;

library ieee;
use ieee.std_logic_1164.all;

entity ozan is
  port (krj : buffer integer; qejn : in string(5 to 5); rschzai : out std_logic_vector(3 downto 0));
end ozan;

architecture ugahohf of ozan is
  
begin
  -- Single-driven assignments
  krj <= 8#1#;
  
  -- Multi-driven assignments
  rschzai <= ('1', 'H', 'U', '-');
  rschzai <= "0-XU";
  rschzai <= ('X', 'H', '-', 'U');
  rschzai <= "HWX-";
end ugahohf;

library ieee;
use ieee.std_logic_1164.all;

entity hvsp is
  port (qye : in std_logic);
end hvsp;

library ieee;
use ieee.std_logic_1164.all;

architecture yxitsujqp of hvsp is
  signal lhrd : std_logic_vector(3 downto 0);
  signal pufuew : string(5 to 5);
  signal va : integer;
  signal sgwaevxgp : boolean;
  signal klcqrxy : std_logic;
  signal d : real;
begin
  rhvjptrvk : entity work.dtgogq
    port map (lpucwyf => d, vrexo => klcqrxy, eabysz => sgwaevxgp);
  aqua : entity work.ozan
    port map (krj => va, qejn => pufuew, rschzai => lhrd);
  
  -- Single-driven assignments
  pufuew <= (others => 'l');
  
  -- Multi-driven assignments
  klcqrxy <= 'X';
end yxitsujqp;



-- Seed after: 12260223322635911323,8421704836678237495
