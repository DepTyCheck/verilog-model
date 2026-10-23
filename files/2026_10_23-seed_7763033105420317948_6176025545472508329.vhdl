-- Seed: 7763033105420317948,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity rpmnie is
  port (pm : in boolean; xmrsugy : inout std_logic);
end rpmnie;

architecture joxzzeoelt of rpmnie is
  
begin
  -- Multi-driven assignments
  xmrsugy <= 'U';
  xmrsugy <= 'H';
  xmrsugy <= 'U';
end joxzzeoelt;

entity cexsaluj is
  port (ujjilouvt : in real);
end cexsaluj;

library ieee;
use ieee.std_logic_1164.all;

architecture ozr of cexsaluj is
  signal pg : std_logic;
  signal jd : boolean;
  signal zwmfk : boolean;
  signal c : std_logic;
  signal eocmsjtog : boolean;
begin
  jmjqxs : entity work.rpmnie
    port map (pm => eocmsjtog, xmrsugy => c);
  dkrwinen : entity work.rpmnie
    port map (pm => zwmfk, xmrsugy => c);
  uaozgkvrpr : entity work.rpmnie
    port map (pm => jd, xmrsugy => c);
  ofijq : entity work.rpmnie
    port map (pm => eocmsjtog, xmrsugy => pg);
  
  -- Single-driven assignments
  eocmsjtog <= zwmfk;
  zwmfk <= eocmsjtog;
  
  -- Multi-driven assignments
  pg <= 'L';
  c <= c;
  c <= '-';
  c <= '0';
end ozr;

library ieee;
use ieee.std_logic_1164.all;

entity fjtkfq is
  port (lnjktee : buffer std_logic_vector(1 to 4); wbkic : inout std_logic_vector(1 downto 1); xvsezwwdjb : linkage string(5 to 4));
end fjtkfq;

library ieee;
use ieee.std_logic_1164.all;

architecture tj of fjtkfq is
  signal ds : real;
  signal iwxbyhzjw : std_logic;
  signal shhr : boolean;
begin
  apdt : entity work.rpmnie
    port map (pm => shhr, xmrsugy => iwxbyhzjw);
  tizwgxei : entity work.cexsaluj
    port map (ujjilouvt => ds);
  b : entity work.cexsaluj
    port map (ujjilouvt => ds);
  
  -- Single-driven assignments
  shhr <= TRUE;
  ds <= ds;
  
  -- Multi-driven assignments
  wbkic <= "Z";
  wbkic <= (others => 'X');
  wbkic <= wbkic;
end tj;



-- Seed after: 13907624033813576210,6176025545472508329
