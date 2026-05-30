-- Seed: 2745349093288044134,1630680796402093529

library ieee;
use ieee.std_logic_1164.all;

entity rrk is
  port (foccxlpn : inout std_logic; nwg : buffer std_logic; mbcwqeh : in time);
end rrk;



architecture eussgwve of rrk is
  
begin
  
end eussgwve;

library ieee;
use ieee.std_logic_1164.all;

entity twekkgjnp is
  port (xvswjfs : buffer std_logic);
end twekkgjnp;

library ieee;
use ieee.std_logic_1164.all;

architecture afilqz of twekkgjnp is
  signal jrf : std_logic;
  signal rbet : std_logic;
  signal nmjxf : time;
  signal uzsh : std_logic;
  signal lejlmbgic : std_logic;
  signal q : time;
  signal pnpy : std_logic;
begin
  qwsvvgflw : entity work.rrk
    port map (foccxlpn => xvswjfs, nwg => pnpy, mbcwqeh => q);
  xdjabf : entity work.rrk
    port map (foccxlpn => lejlmbgic, nwg => uzsh, mbcwqeh => q);
  aefsh : entity work.rrk
    port map (foccxlpn => xvswjfs, nwg => lejlmbgic, mbcwqeh => nmjxf);
  cgb : entity work.rrk
    port map (foccxlpn => rbet, nwg => jrf, mbcwqeh => q);
end afilqz;



entity ydh is
  port (pukc : in time_vector(2 downto 3));
end ydh;

library ieee;
use ieee.std_logic_1164.all;

architecture kxjtxig of ydh is
  signal jvsawcfn : time;
  signal baoz : std_logic;
begin
  ocn : entity work.rrk
    port map (foccxlpn => baoz, nwg => baoz, mbcwqeh => jvsawcfn);
end kxjtxig;



entity xrw is
  port (hffwkmtbbl : out integer_vector(3 to 4));
end xrw;

library ieee;
use ieee.std_logic_1164.all;

architecture ehzcuonjzj of xrw is
  signal uywrahjno : time_vector(2 downto 3);
  signal coger : time;
  signal ie : std_logic;
  signal tnnefpi : std_logic;
begin
  boyo : entity work.rrk
    port map (foccxlpn => tnnefpi, nwg => ie, mbcwqeh => coger);
  xfq : entity work.ydh
    port map (pukc => uywrahjno);
  u : entity work.twekkgjnp
    port map (xvswjfs => ie);
end ehzcuonjzj;



-- Seed after: 17362723254306146452,1630680796402093529
