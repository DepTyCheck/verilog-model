-- Seed: 17978130626831599226,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity gn is
  port (uzrajmojt : linkage integer_vector(0 to 2); okdxeh : inout std_logic_vector(0 to 1));
end gn;

architecture kq of gn is
  
begin
  -- Multi-driven assignments
  okdxeh <= ('X', 'Z');
  okdxeh <= ('L', '-');
end kq;

entity iad is
  port (y : inout real_vector(3 downto 1); netio : inout bit_vector(4 to 0));
end iad;

library ieee;
use ieee.std_logic_1164.all;

architecture mrtrozbtqr of iad is
  signal bqcjso : integer_vector(0 to 2);
  signal yex : std_logic_vector(0 to 1);
  signal xblksovy : integer_vector(0 to 2);
  signal ygwuwkmhr : std_logic_vector(0 to 1);
  signal cfiy : integer_vector(0 to 2);
begin
  mdppeijn : entity work.gn
    port map (uzrajmojt => cfiy, okdxeh => ygwuwkmhr);
  k : entity work.gn
    port map (uzrajmojt => xblksovy, okdxeh => yex);
  rpbuucqiy : entity work.gn
    port map (uzrajmojt => bqcjso, okdxeh => ygwuwkmhr);
  
  -- Multi-driven assignments
  ygwuwkmhr <= "0W";
  ygwuwkmhr <= ygwuwkmhr;
  ygwuwkmhr <= yex;
end mrtrozbtqr;



-- Seed after: 127856564775303971,6379010654866854599
