-- Seed: 18100392121074798049,7311216359267151659

entity qujz is
  port (gdv : in boolean; edlagoaa : buffer boolean_vector(3 to 3));
end qujz;

architecture reouxrsr of qujz is
  
begin
  -- Single-driven assignments
  edlagoaa <= edlagoaa;
end reouxrsr;

library ieee;
use ieee.std_logic_1164.all;

entity ojxuspaoq is
  port (pcxo : in std_logic; ggbny : out integer; nkjht : inout std_logic_vector(1 to 4));
end ojxuspaoq;

architecture qgciuevrnu of ojxuspaoq is
  signal cpbciqe : boolean_vector(3 to 3);
  signal tjt : boolean;
  signal ehsvrosi : boolean_vector(3 to 3);
  signal ixqw : boolean;
  signal zlxxajrkk : boolean_vector(3 to 3);
  signal dqoltazmeq : boolean;
begin
  m : entity work.qujz
    port map (gdv => dqoltazmeq, edlagoaa => zlxxajrkk);
  rn : entity work.qujz
    port map (gdv => ixqw, edlagoaa => ehsvrosi);
  hvzykwurau : entity work.qujz
    port map (gdv => tjt, edlagoaa => cpbciqe);
  
  -- Multi-driven assignments
  nkjht <= "1WH-";
  nkjht <= nkjht;
  nkjht <= ('0', 'X', 'W', 'Z');
end qgciuevrnu;

library ieee;
use ieee.std_logic_1164.all;

entity wmit is
  port (fnbgum : inout std_logic_vector(4 downto 2); s : buffer time; hmgiyrj : inout integer);
end wmit;

library ieee;
use ieee.std_logic_1164.all;

architecture dxppazro of wmit is
  signal qlhhoxis : boolean_vector(3 to 3);
  signal jcniks : boolean;
  signal atr : std_logic_vector(1 to 4);
  signal mld : std_logic_vector(1 to 4);
  signal ivybvhc : integer;
  signal nsuarp : std_logic;
begin
  i : entity work.ojxuspaoq
    port map (pcxo => nsuarp, ggbny => ivybvhc, nkjht => mld);
  jnsxtzcv : entity work.ojxuspaoq
    port map (pcxo => nsuarp, ggbny => hmgiyrj, nkjht => atr);
  huhxhauz : entity work.qujz
    port map (gdv => jcniks, edlagoaa => qlhhoxis);
  
  -- Single-driven assignments
  s <= 4_3_3.2_4 fs;
  jcniks <= jcniks;
end dxppazro;



-- Seed after: 8730165170490315297,7311216359267151659
