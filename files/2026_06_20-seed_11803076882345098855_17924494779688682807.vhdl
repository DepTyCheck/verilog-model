-- Seed: 11803076882345098855,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity ctu is
  port (trvqey : buffer std_logic_vector(2 to 1); voual : out integer; opxwst : buffer real; kmmy : out boolean);
end ctu;

architecture wtkai of ctu is
  
begin
  -- Single-driven assignments
  opxwst <= 0.0_1_4;
  kmmy <= FALSE;
  voual <= 1_1_1_2_2;
end wtkai;

library ieee;
use ieee.std_logic_1164.all;

entity qxprnhud is
  port (szqu : in std_logic_vector(2 downto 3); uydvvcjypu : inout bit; c : out time);
end qxprnhud;

library ieee;
use ieee.std_logic_1164.all;

architecture pguisvjmg of qxprnhud is
  signal wqckwzlrv : boolean;
  signal lwcjumh : real;
  signal dijcdxhlj : integer;
  signal ibsdcrftoo : std_logic_vector(2 to 1);
begin
  lgved : entity work.ctu
    port map (trvqey => ibsdcrftoo, voual => dijcdxhlj, opxwst => lwcjumh, kmmy => wqckwzlrv);
  
  -- Multi-driven assignments
  ibsdcrftoo <= "";
end pguisvjmg;

entity vggpzzr is
  port (gdjpaxkddh : inout real; jhb : out integer);
end vggpzzr;

library ieee;
use ieee.std_logic_1164.all;

architecture iepwjkf of vggpzzr is
  signal rpbtfgfcmp : boolean;
  signal rpbhcfdypk : real;
  signal awkkvlk : std_logic_vector(2 to 1);
begin
  v : entity work.ctu
    port map (trvqey => awkkvlk, voual => jhb, opxwst => rpbhcfdypk, kmmy => rpbtfgfcmp);
  
  -- Single-driven assignments
  gdjpaxkddh <= 2#11010.01001#;
  
  -- Multi-driven assignments
  awkkvlk <= (others => '0');
  awkkvlk <= "";
end iepwjkf;

library ieee;
use ieee.std_logic_1164.all;

entity lnidvhivjo is
  port (crzyus : buffer std_logic_vector(0 to 4); pqijsjtsyf : out std_logic);
end lnidvhivjo;

library ieee;
use ieee.std_logic_1164.all;

architecture gq of lnidvhivjo is
  signal hwnecrrym : time;
  signal ipauqic : bit;
  signal rymgyijxr : std_logic_vector(2 downto 3);
  signal wm : boolean;
  signal w : real;
  signal skz : integer;
  signal ull : boolean;
  signal ifhyo : real;
  signal ujlcep : integer;
  signal pcvqvb : std_logic_vector(2 to 1);
begin
  whfthlrku : entity work.ctu
    port map (trvqey => pcvqvb, voual => ujlcep, opxwst => ifhyo, kmmy => ull);
  yqf : entity work.ctu
    port map (trvqey => pcvqvb, voual => skz, opxwst => w, kmmy => wm);
  iztrrav : entity work.qxprnhud
    port map (szqu => rymgyijxr, uydvvcjypu => ipauqic, c => hwnecrrym);
  
  -- Multi-driven assignments
  pqijsjtsyf <= '1';
  rymgyijxr <= "";
  pqijsjtsyf <= '-';
end gq;



-- Seed after: 8438445878079567563,17924494779688682807
