-- Seed: 1098458366736529938,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity mkodjtfd is
  port (cyir : inout time; tap : inout std_logic_vector(4 downto 3));
end mkodjtfd;

architecture x of mkodjtfd is
  
begin
  -- Single-driven assignments
  cyir <= cyir;
  
  -- Multi-driven assignments
  tap <= ('W', 'H');
  tap <= "ZU";
  tap <= ('0', 'L');
  tap <= "10";
end x;

library ieee;
use ieee.std_logic_1164.all;

entity djdp is
  port (oyaikbtyu : buffer std_logic; ldgpgsfel : out real; prbhrllq : out std_logic_vector(3 to 2));
end djdp;

architecture pw of djdp is
  
begin
  -- Multi-driven assignments
  oyaikbtyu <= oyaikbtyu;
  prbhrllq <= prbhrllq;
  prbhrllq <= prbhrllq;
end pw;

entity xkjcpvlan is
  port (qtvlrg : in bit_vector(1 to 2); tyaciix : out real);
end xkjcpvlan;

library ieee;
use ieee.std_logic_1164.all;

architecture exshu of xkjcpvlan is
  signal xbqlm : time;
  signal g : time;
  signal qfbhzfsisb : std_logic_vector(4 downto 3);
  signal url : time;
  signal cga : std_logic_vector(3 to 2);
  signal zz : real;
  signal qvha : std_logic;
begin
  kvkqfyctr : entity work.djdp
    port map (oyaikbtyu => qvha, ldgpgsfel => zz, prbhrllq => cga);
  tx : entity work.mkodjtfd
    port map (cyir => url, tap => qfbhzfsisb);
  okvzsjmzjt : entity work.mkodjtfd
    port map (cyir => g, tap => qfbhzfsisb);
  uae : entity work.mkodjtfd
    port map (cyir => xbqlm, tap => qfbhzfsisb);
  
  -- Single-driven assignments
  tyaciix <= 13.1302;
  
  -- Multi-driven assignments
  qvha <= qvha;
  qvha <= 'W';
  qfbhzfsisb <= ('H', 'L');
end exshu;

entity ocyhuaves is
  port (gbwrn : inout integer; itasznzbs : out time);
end ocyhuaves;

library ieee;
use ieee.std_logic_1164.all;

architecture z of ocyhuaves is
  signal wkpwly : real;
  signal xdsfhfe : bit_vector(1 to 2);
  signal rdyvj : std_logic_vector(4 downto 3);
  signal wvlglbd : std_logic_vector(4 downto 3);
  signal yujqmy : time;
  signal wimwpb : std_logic_vector(4 downto 3);
  signal yowugmkmvk : time;
begin
  kxyn : entity work.mkodjtfd
    port map (cyir => yowugmkmvk, tap => wimwpb);
  vurrq : entity work.mkodjtfd
    port map (cyir => yujqmy, tap => wvlglbd);
  boudv : entity work.mkodjtfd
    port map (cyir => itasznzbs, tap => rdyvj);
  gpllokkq : entity work.xkjcpvlan
    port map (qtvlrg => xdsfhfe, tyaciix => wkpwly);
  
  -- Single-driven assignments
  gbwrn <= gbwrn;
  xdsfhfe <= xdsfhfe;
  
  -- Multi-driven assignments
  wimwpb <= ('X', 'W');
  wimwpb <= wimwpb;
end z;



-- Seed after: 10966202895049450306,511364357853360275
