-- Seed: 16409563599240930216,511364357853360275

entity dkxxzi is
  port (xpxdjbw : linkage integer);
end dkxxzi;

architecture bwqy of dkxxzi is
  
begin
  
end bwqy;

library ieee;
use ieee.std_logic_1164.all;

entity bkmev is
  port (p : buffer std_logic; iujbvrt : out std_logic_vector(3 downto 1); wdhp : linkage std_logic; jwkgsqblxp : inout real);
end bkmev;

architecture ki of bkmev is
  signal ywva : integer;
  signal hlgupgswfi : integer;
begin
  dm : entity work.dkxxzi
    port map (xpxdjbw => hlgupgswfi);
  grvlcefqa : entity work.dkxxzi
    port map (xpxdjbw => ywva);
  
  -- Multi-driven assignments
  iujbvrt <= iujbvrt;
  p <= 'Z';
  iujbvrt <= ('H', 'H', '1');
end ki;

library ieee;
use ieee.std_logic_1164.all;

entity iasscmt is
  port (pilnwqkq : out std_logic_vector(1 downto 4); gvwhwhxfp : linkage std_logic_vector(0 to 0));
end iasscmt;

library ieee;
use ieee.std_logic_1164.all;

architecture plkbnfrtph of iasscmt is
  signal kprtk : real;
  signal bs : std_logic;
  signal g : real;
  signal xnzi : std_logic;
  signal qa : std_logic_vector(3 downto 1);
  signal fhoqp : std_logic;
begin
  hdp : entity work.bkmev
    port map (p => fhoqp, iujbvrt => qa, wdhp => xnzi, jwkgsqblxp => g);
  ejqfzv : entity work.bkmev
    port map (p => fhoqp, iujbvrt => qa, wdhp => bs, jwkgsqblxp => kprtk);
  
  -- Multi-driven assignments
  pilnwqkq <= pilnwqkq;
  pilnwqkq <= (others => '0');
end plkbnfrtph;

entity quz is
  port (jvis : inout integer; azvoomukki : inout integer; phcewvu : in severity_level);
end quz;

library ieee;
use ieee.std_logic_1164.all;

architecture ulwy of quz is
  signal zveyxmvd : integer;
  signal jhiub : std_logic_vector(0 to 0);
  signal ybwl : std_logic_vector(1 downto 4);
begin
  unnxzift : entity work.iasscmt
    port map (pilnwqkq => ybwl, gvwhwhxfp => jhiub);
  gvewg : entity work.dkxxzi
    port map (xpxdjbw => zveyxmvd);
  
  -- Single-driven assignments
  azvoomukki <= azvoomukki;
  jvis <= 16#7_1#;
  
  -- Multi-driven assignments
  ybwl <= (others => '0');
  jhiub <= jhiub;
  jhiub <= (others => 'L');
  ybwl <= ybwl;
end ulwy;



-- Seed after: 9793972544265562418,511364357853360275
