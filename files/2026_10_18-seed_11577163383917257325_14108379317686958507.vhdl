-- Seed: 11577163383917257325,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity xwuakq is
  port (rtowynxfjc : out std_logic; pdwyzfpxb : linkage integer_vector(3 downto 4); ndts : inout time; rwxmdwh : linkage std_logic);
end xwuakq;

architecture t of xwuakq is
  
begin
  -- Single-driven assignments
  ndts <= 4_1_4 ps;
  
  -- Multi-driven assignments
  rtowynxfjc <= rtowynxfjc;
end t;

library ieee;
use ieee.std_logic_1164.all;

entity pvcqqmohjt is
  port (cn : out std_logic_vector(0 to 1); vpl : buffer std_logic_vector(2 to 0); f : inout boolean_vector(0 to 2); zdu : linkage std_logic);
end pvcqqmohjt;

library ieee;
use ieee.std_logic_1164.all;

architecture qtyspzdjp of pvcqqmohjt is
  signal dq : time;
  signal dro : integer_vector(3 downto 4);
  signal rjrzx : std_logic;
  signal bv : time;
  signal jazv : integer_vector(3 downto 4);
  signal okchxgfdy : std_logic;
  signal ddunflp : std_logic;
  signal sxkstuhehv : time;
  signal mc : integer_vector(3 downto 4);
  signal yd : std_logic;
  signal evjc : time;
  signal hxepl : integer_vector(3 downto 4);
  signal opwftvrj : std_logic;
begin
  ovv : entity work.xwuakq
    port map (rtowynxfjc => opwftvrj, pdwyzfpxb => hxepl, ndts => evjc, rwxmdwh => zdu);
  fmjkahr : entity work.xwuakq
    port map (rtowynxfjc => yd, pdwyzfpxb => mc, ndts => sxkstuhehv, rwxmdwh => ddunflp);
  evffk : entity work.xwuakq
    port map (rtowynxfjc => okchxgfdy, pdwyzfpxb => jazv, ndts => bv, rwxmdwh => rjrzx);
  rvovaihy : entity work.xwuakq
    port map (rtowynxfjc => opwftvrj, pdwyzfpxb => dro, ndts => dq, rwxmdwh => zdu);
  
  -- Single-driven assignments
  f <= (FALSE, TRUE, TRUE);
  
  -- Multi-driven assignments
  ddunflp <= opwftvrj;
end qtyspzdjp;

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (fyswsv : out std_logic; ettrlrlljr : in std_logic; hrbvk : linkage time);
end l;

library ieee;
use ieee.std_logic_1164.all;

architecture jnjozmjy of l is
  signal snhwiq : time;
  signal f : integer_vector(3 downto 4);
  signal ivwt : boolean_vector(0 to 2);
  signal bgkiabuut : std_logic_vector(2 to 0);
  signal iakfruygwo : std_logic_vector(0 to 1);
begin
  jmrh : entity work.pvcqqmohjt
    port map (cn => iakfruygwo, vpl => bgkiabuut, f => ivwt, zdu => ettrlrlljr);
  xjs : entity work.xwuakq
    port map (rtowynxfjc => fyswsv, pdwyzfpxb => f, ndts => snhwiq, rwxmdwh => fyswsv);
  
  -- Multi-driven assignments
  fyswsv <= 'L';
end jnjozmjy;



-- Seed after: 11993955453884050110,14108379317686958507
