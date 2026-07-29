-- Seed: 13956107177263247229,14641901754878719179

entity kfiqgij is
  port (atm : out bit; urgo : out bit_vector(2 to 1));
end kfiqgij;

architecture spuxgemlja of kfiqgij is
  
begin
  -- Single-driven assignments
  urgo <= (others => '0');
  atm <= '0';
end spuxgemlja;

library ieee;
use ieee.std_logic_1164.all;

entity cc is
  port (jbnlew : inout std_logic);
end cc;

architecture sljzmuxl of cc is
  signal mtfcnzy : bit_vector(2 to 1);
  signal difeylg : bit;
  signal noao : bit_vector(2 to 1);
  signal utpgsxeh : bit;
begin
  vssv : entity work.kfiqgij
    port map (atm => utpgsxeh, urgo => noao);
  h : entity work.kfiqgij
    port map (atm => difeylg, urgo => mtfcnzy);
  
  -- Multi-driven assignments
  jbnlew <= jbnlew;
  jbnlew <= 'Z';
  jbnlew <= jbnlew;
  jbnlew <= jbnlew;
end sljzmuxl;

entity psg is
  port (wudedscbf : inout time);
end psg;

architecture qslsdmm of psg is
  signal ndvzed : bit_vector(2 to 1);
  signal apva : bit;
begin
  rwo : entity work.kfiqgij
    port map (atm => apva, urgo => ndvzed);
  
  -- Single-driven assignments
  wudedscbf <= wudedscbf;
end qslsdmm;



-- Seed after: 850877571731292118,14641901754878719179
