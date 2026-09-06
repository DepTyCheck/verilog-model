-- Seed: 15560657316458249573,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity dslg is
  port (wawaxw : in std_logic_vector(4 to 3); jtbtcii : in time; tqjcuqffhu : inout std_logic_vector(0 to 1));
end dslg;

architecture ovijt of dslg is
  
begin
  -- Multi-driven assignments
  tqjcuqffhu <= "ZH";
  tqjcuqffhu <= tqjcuqffhu;
end ovijt;

entity o is
  port (pqlsvorax : buffer string(5 to 5); kse : in integer);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture weyjgzzzq of o is
  signal pzydvu : std_logic_vector(0 to 1);
  signal ymbs : time;
  signal jq : time;
  signal gabhw : std_logic_vector(4 to 3);
  signal nbe : std_logic_vector(0 to 1);
  signal wespczkkn : time;
  signal gr : std_logic_vector(4 to 3);
begin
  awaqyv : entity work.dslg
    port map (wawaxw => gr, jtbtcii => wespczkkn, tqjcuqffhu => nbe);
  nynvm : entity work.dslg
    port map (wawaxw => gabhw, jtbtcii => jq, tqjcuqffhu => nbe);
  uj : entity work.dslg
    port map (wawaxw => gr, jtbtcii => ymbs, tqjcuqffhu => pzydvu);
  i : entity work.dslg
    port map (wawaxw => gr, jtbtcii => wespczkkn, tqjcuqffhu => pzydvu);
  
  -- Single-driven assignments
  pqlsvorax <= "m";
  ymbs <= 30 ps;
  jq <= 8#6_4# us;
  wespczkkn <= ymbs;
  
  -- Multi-driven assignments
  gr <= (others => '0');
  nbe <= nbe;
end weyjgzzzq;



-- Seed after: 2501787587417266313,14094562573555574003
