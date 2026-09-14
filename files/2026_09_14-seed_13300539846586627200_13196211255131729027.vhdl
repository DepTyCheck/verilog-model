-- Seed: 13300539846586627200,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity qo is
  port (loqpdgxyy : inout time; ihhflam : inout std_logic_vector(0 to 0));
end qo;

architecture ojbzzr of qo is
  
begin
  -- Multi-driven assignments
  ihhflam <= (others => 'Z');
  ihhflam <= ihhflam;
  ihhflam <= (others => 'X');
end ojbzzr;

entity jyjyxomwg is
  port (mw : in real_vector(2 downto 2); tyuaacyr : buffer bit);
end jyjyxomwg;

library ieee;
use ieee.std_logic_1164.all;

architecture behl of jyjyxomwg is
  signal vkk : std_logic_vector(0 to 0);
  signal gcww : time;
  signal mg : std_logic_vector(0 to 0);
  signal kpdfhluqi : time;
  signal coibv : time;
  signal oygyskby : std_logic_vector(0 to 0);
  signal esnlblfb : time;
begin
  xqsheozih : entity work.qo
    port map (loqpdgxyy => esnlblfb, ihhflam => oygyskby);
  qmnp : entity work.qo
    port map (loqpdgxyy => coibv, ihhflam => oygyskby);
  bzeew : entity work.qo
    port map (loqpdgxyy => kpdfhluqi, ihhflam => mg);
  ajrlmbg : entity work.qo
    port map (loqpdgxyy => gcww, ihhflam => vkk);
  
  -- Single-driven assignments
  tyuaacyr <= '1';
end behl;



-- Seed after: 13389580360241700160,13196211255131729027
