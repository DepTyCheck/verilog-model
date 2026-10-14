-- Seed: 7400602985350877772,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity zinyzf is
  port (qlnaipk : in std_logic_vector(3 to 0); c : inout character; akxqe : buffer std_logic; bwnabn : buffer std_logic_vector(1 downto 3));
end zinyzf;

architecture wpq of zinyzf is
  
begin
  -- Single-driven assignments
  c <= 'j';
  
  -- Multi-driven assignments
  bwnabn <= "";
  akxqe <= 'X';
end wpq;

entity ecidvpjn is
  port (lyb : out integer; xljnrn : out time);
end ecidvpjn;

library ieee;
use ieee.std_logic_1164.all;

architecture ujelpqe of ecidvpjn is
  signal szl : std_logic;
  signal nvkpvps : character;
  signal nttjwglaex : std_logic_vector(3 to 0);
  signal rjveuw : std_logic_vector(1 downto 3);
  signal cwie : std_logic;
  signal tsqgom : character;
  signal f : std_logic_vector(3 to 0);
begin
  epxryniq : entity work.zinyzf
    port map (qlnaipk => f, c => tsqgom, akxqe => cwie, bwnabn => rjveuw);
  pyfumno : entity work.zinyzf
    port map (qlnaipk => nttjwglaex, c => nvkpvps, akxqe => szl, bwnabn => rjveuw);
  
  -- Multi-driven assignments
  f <= f;
  f <= "";
end ujelpqe;

entity ictjgahi is
  port (fiaxa : inout time; wvankokl : in integer; mbgs : in real; ftgbqfrsq : buffer time);
end ictjgahi;

library ieee;
use ieee.std_logic_1164.all;

architecture mn of ictjgahi is
  signal rnixzwllol : std_logic;
  signal pjiobp : character;
  signal ihwnvxzi : std_logic_vector(1 downto 3);
  signal wap : integer;
  signal ouzssl : time;
  signal pdqqqsllt : integer;
begin
  o : entity work.ecidvpjn
    port map (lyb => pdqqqsllt, xljnrn => ouzssl);
  h : entity work.ecidvpjn
    port map (lyb => wap, xljnrn => ftgbqfrsq);
  tqxwkhwgmj : entity work.zinyzf
    port map (qlnaipk => ihwnvxzi, c => pjiobp, akxqe => rnixzwllol, bwnabn => ihwnvxzi);
  
  -- Single-driven assignments
  fiaxa <= 323.21 us;
  
  -- Multi-driven assignments
  ihwnvxzi <= ihwnvxzi;
end mn;



-- Seed after: 2672610673133250841,2287986438313874379
