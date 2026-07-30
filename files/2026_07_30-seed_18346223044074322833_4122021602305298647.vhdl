-- Seed: 18346223044074322833,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity iemadqe is
  port (vffr : buffer std_logic; u : buffer integer; hwyofqjnr : inout std_logic; uea : buffer std_logic);
end iemadqe;

architecture fhc of iemadqe is
  
begin
  -- Single-driven assignments
  u <= u;
  
  -- Multi-driven assignments
  uea <= 'X';
  vffr <= hwyofqjnr;
  vffr <= 'U';
  uea <= '0';
end fhc;

entity nqgewqai is
  port (oljfi : inout boolean_vector(2 downto 3));
end nqgewqai;

library ieee;
use ieee.std_logic_1164.all;

architecture poy of nqgewqai is
  signal nkugau : std_logic;
  signal acgpk : integer;
  signal hjfndz : std_logic;
  signal t : std_logic;
  signal jja : integer;
  signal oqfnzowh : std_logic;
  signal sur : std_logic;
  signal ubvae : integer;
  signal ewjeqi : std_logic;
  signal ihpyckvtqu : std_logic;
  signal e : integer;
  signal yhumbj : std_logic;
begin
  mbufzal : entity work.iemadqe
    port map (vffr => yhumbj, u => e, hwyofqjnr => ihpyckvtqu, uea => yhumbj);
  petiol : entity work.iemadqe
    port map (vffr => ewjeqi, u => ubvae, hwyofqjnr => sur, uea => yhumbj);
  pbs : entity work.iemadqe
    port map (vffr => oqfnzowh, u => jja, hwyofqjnr => t, uea => hjfndz);
  f : entity work.iemadqe
    port map (vffr => yhumbj, u => acgpk, hwyofqjnr => nkugau, uea => yhumbj);
  
  -- Single-driven assignments
  oljfi <= (others => TRUE);
  
  -- Multi-driven assignments
  yhumbj <= '0';
  ewjeqi <= '1';
  t <= 'X';
end poy;



-- Seed after: 17741941381128309441,4122021602305298647
