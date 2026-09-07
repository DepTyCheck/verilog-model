-- Seed: 6780562478963405705,12269339630485015285

entity nhdf is
  port (ghfsxeotj : in time_vector(3 downto 0); g : in integer);
end nhdf;

architecture shvcwa of nhdf is
  
begin
  
end shvcwa;

library ieee;
use ieee.std_logic_1164.all;

entity kweuttai is
  port (ispuqe : in time_vector(3 to 2); xmptxo : inout severity_level; ealkjfd : out std_logic; gawmj : in boolean);
end kweuttai;

architecture rmkren of kweuttai is
  signal qvug : integer;
  signal opuikmsg : integer;
  signal hryocukg : time_vector(3 downto 0);
  signal fkgxvpmk : integer;
  signal hgm : time_vector(3 downto 0);
  signal lpamvv : integer;
  signal ugycajl : time_vector(3 downto 0);
begin
  dlelk : entity work.nhdf
    port map (ghfsxeotj => ugycajl, g => lpamvv);
  hhjg : entity work.nhdf
    port map (ghfsxeotj => hgm, g => fkgxvpmk);
  oqflnl : entity work.nhdf
    port map (ghfsxeotj => hryocukg, g => opuikmsg);
  aeuhn : entity work.nhdf
    port map (ghfsxeotj => ugycajl, g => qvug);
end rmkren;

library ieee;
use ieee.std_logic_1164.all;

entity casjcijs is
  port (xpep : out std_logic_vector(2 downto 2); li : in integer; lofqv : inout boolean);
end casjcijs;

library ieee;
use ieee.std_logic_1164.all;

architecture ofkntcyfg of casjcijs is
  signal ljsp : boolean;
  signal qezqvy : std_logic;
  signal h : severity_level;
  signal xellgjohi : time_vector(3 to 2);
  signal mwbmblmyb : time_vector(3 downto 0);
  signal urqhjo : time_vector(3 downto 0);
begin
  fbzjjn : entity work.nhdf
    port map (ghfsxeotj => urqhjo, g => li);
  iz : entity work.nhdf
    port map (ghfsxeotj => mwbmblmyb, g => li);
  ae : entity work.nhdf
    port map (ghfsxeotj => urqhjo, g => li);
  fjavcdotf : entity work.kweuttai
    port map (ispuqe => xellgjohi, xmptxo => h, ealkjfd => qezqvy, gawmj => ljsp);
  
  -- Multi-driven assignments
  xpep <= (others => 'X');
  xpep <= "1";
  xpep <= "-";
  qezqvy <= 'U';
end ofkntcyfg;

entity oajlseij is
  port (nfnpfjmnm : linkage time; xukqxvjjn : inout time);
end oajlseij;

library ieee;
use ieee.std_logic_1164.all;

architecture pwmfnpktob of oajlseij is
  signal vgjdqo : integer;
  signal qgl : time_vector(3 downto 0);
  signal sz : boolean;
  signal zldjz : integer;
  signal o : std_logic_vector(2 downto 2);
begin
  re : entity work.casjcijs
    port map (xpep => o, li => zldjz, lofqv => sz);
  icgz : entity work.nhdf
    port map (ghfsxeotj => qgl, g => vgjdqo);
  pbyorayw : entity work.nhdf
    port map (ghfsxeotj => qgl, g => zldjz);
  
  -- Single-driven assignments
  xukqxvjjn <= 8#1_1_2_0_1# ms;
  
  -- Multi-driven assignments
  o <= "X";
  o <= (others => '0');
  o <= "Z";
  o <= (others => '1');
end pwmfnpktob;



-- Seed after: 12273159196125780908,12269339630485015285
