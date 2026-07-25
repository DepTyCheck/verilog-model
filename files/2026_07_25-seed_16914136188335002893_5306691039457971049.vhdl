-- Seed: 16914136188335002893,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity gdqyohvsy is
  port (wevnvx : buffer bit; nfqynkgvej : in std_logic; haosqymrtu : linkage std_logic_vector(2 downto 4));
end gdqyohvsy;

architecture xmaya of gdqyohvsy is
  
begin
  -- Single-driven assignments
  wevnvx <= '0';
end xmaya;

library ieee;
use ieee.std_logic_1164.all;

entity vgfb is
  port (xf : buffer std_logic_vector(1 downto 3); qrvcwcjc : out bit_vector(4 downto 0));
end vgfb;

library ieee;
use ieee.std_logic_1164.all;

architecture lcx of vgfb is
  signal wayycx : std_logic;
  signal epqaxqgdaq : bit;
  signal fvlnhryg : std_logic_vector(2 downto 4);
  signal akbgaiur : std_logic;
  signal w : bit;
begin
  nnscuyw : entity work.gdqyohvsy
    port map (wevnvx => w, nfqynkgvej => akbgaiur, haosqymrtu => fvlnhryg);
  lhsnn : entity work.gdqyohvsy
    port map (wevnvx => epqaxqgdaq, nfqynkgvej => wayycx, haosqymrtu => xf);
  
  -- Single-driven assignments
  qrvcwcjc <= ('0', '0', '0', '0', '1');
  
  -- Multi-driven assignments
  fvlnhryg <= xf;
  akbgaiur <= '0';
  fvlnhryg <= "";
  wayycx <= akbgaiur;
end lcx;

library ieee;
use ieee.std_logic_1164.all;

entity imuv is
  port (rekuq : out std_logic_vector(0 downto 3));
end imuv;

library ieee;
use ieee.std_logic_1164.all;

architecture cokhqau of imuv is
  signal lfpijio : std_logic_vector(2 downto 4);
  signal kz : bit;
  signal xbbqmas : std_logic_vector(2 downto 4);
  signal juizsu : std_logic;
  signal badvifvcg : bit;
begin
  rt : entity work.gdqyohvsy
    port map (wevnvx => badvifvcg, nfqynkgvej => juizsu, haosqymrtu => xbbqmas);
  jnux : entity work.gdqyohvsy
    port map (wevnvx => kz, nfqynkgvej => juizsu, haosqymrtu => lfpijio);
  
  -- Multi-driven assignments
  xbbqmas <= "";
  rekuq <= rekuq;
  rekuq <= rekuq;
  xbbqmas <= lfpijio;
end cokhqau;

library ieee;
use ieee.std_logic_1164.all;

entity geiqcy is
  port (lnyohh : linkage time; dxput : linkage std_logic; r : in std_logic_vector(3 to 2); nnqexaba : linkage bit);
end geiqcy;

library ieee;
use ieee.std_logic_1164.all;

architecture xxixnkax of geiqcy is
  signal ujdrgquwj : std_logic;
  signal losjlphytk : bit;
  signal xulsabjifp : std_logic_vector(2 downto 4);
  signal idvpoy : std_logic;
  signal cxemka : bit;
  signal nvnhmhb : std_logic_vector(2 downto 4);
begin
  rgz : entity work.imuv
    port map (rekuq => nvnhmhb);
  endftjvej : entity work.gdqyohvsy
    port map (wevnvx => cxemka, nfqynkgvej => idvpoy, haosqymrtu => nvnhmhb);
  wszaordvwc : entity work.imuv
    port map (rekuq => xulsabjifp);
  jsovkjbf : entity work.gdqyohvsy
    port map (wevnvx => losjlphytk, nfqynkgvej => ujdrgquwj, haosqymrtu => xulsabjifp);
end xxixnkax;



-- Seed after: 3325777413906268691,5306691039457971049
