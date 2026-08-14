-- Seed: 7678562031418219837,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (hwm : buffer boolean_vector(0 to 3); dgl : linkage std_logic; ecjiijsl : inout real_vector(1 downto 0); b : in boolean_vector(2 to 2));
end l;

architecture pfgecbf of l is
  
begin
  -- Single-driven assignments
  ecjiijsl <= (3141.3, 2#0_0.10000#);
  hwm <= (FALSE, FALSE, FALSE, FALSE);
end pfgecbf;

entity nyst is
  port (gseqb : buffer severity_level);
end nyst;

architecture txdztwff of nyst is
  
begin
  
end txdztwff;

library ieee;
use ieee.std_logic_1164.all;

entity lxfkzngswh is
  port (kmdgyym : out std_logic_vector(3 downto 2); fr : buffer real; rxs : buffer boolean);
end lxfkzngswh;

architecture cad of lxfkzngswh is
  
begin
  -- Single-driven assignments
  rxs <= FALSE;
  
  -- Multi-driven assignments
  kmdgyym <= kmdgyym;
  kmdgyym <= ('Z', 'H');
  kmdgyym <= ('U', 'Z');
end cad;

entity qiym is
  port (wk : inout bit);
end qiym;

library ieee;
use ieee.std_logic_1164.all;

architecture shzmhiuhs of qiym is
  signal tjq : severity_level;
  signal mvbzpvvt : boolean;
  signal tuxbiv : real;
  signal kqaylptvgt : std_logic_vector(3 downto 2);
  signal ar : boolean_vector(2 to 2);
  signal zqofmtmz : real_vector(1 downto 0);
  signal btypfn : std_logic;
  signal dsvf : boolean_vector(0 to 3);
  signal oprtutxnt : severity_level;
begin
  d : entity work.nyst
    port map (gseqb => oprtutxnt);
  okho : entity work.l
    port map (hwm => dsvf, dgl => btypfn, ecjiijsl => zqofmtmz, b => ar);
  o : entity work.lxfkzngswh
    port map (kmdgyym => kqaylptvgt, fr => tuxbiv, rxs => mvbzpvvt);
  vemwfwk : entity work.nyst
    port map (gseqb => tjq);
  
  -- Single-driven assignments
  wk <= wk;
  ar <= (others => FALSE);
  
  -- Multi-driven assignments
  kqaylptvgt <= ('Z', '-');
  btypfn <= btypfn;
end shzmhiuhs;



-- Seed after: 2022732784355816170,8437298063418820479
