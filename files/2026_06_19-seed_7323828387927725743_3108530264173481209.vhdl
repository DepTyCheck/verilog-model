-- Seed: 7323828387927725743,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity ao is
  port (cxlcoqmu : buffer std_logic);
end ao;

architecture re of ao is
  
begin
  -- Multi-driven assignments
  cxlcoqmu <= 'Z';
  cxlcoqmu <= 'X';
  cxlcoqmu <= '-';
end re;

entity e is
  port (apvpdsxzrm : inout bit_vector(4 to 0));
end e;

architecture tinlhfhv of e is
  
begin
  -- Single-driven assignments
  apvpdsxzrm <= (others => '0');
end tinlhfhv;

library ieee;
use ieee.std_logic_1164.all;

entity jhm is
  port (wiluahjs : inout std_logic);
end jhm;

architecture urt of jhm is
  signal tcxh : bit_vector(4 to 0);
  signal acvgteeeof : bit_vector(4 to 0);
begin
  dbsuzhkz : entity work.ao
    port map (cxlcoqmu => wiluahjs);
  w : entity work.e
    port map (apvpdsxzrm => acvgteeeof);
  tqft : entity work.e
    port map (apvpdsxzrm => tcxh);
  tnww : entity work.ao
    port map (cxlcoqmu => wiluahjs);
  
  -- Multi-driven assignments
  wiluahjs <= '0';
  wiluahjs <= 'X';
  wiluahjs <= 'X';
  wiluahjs <= 'H';
end urt;



-- Seed after: 17062458579513004386,3108530264173481209
