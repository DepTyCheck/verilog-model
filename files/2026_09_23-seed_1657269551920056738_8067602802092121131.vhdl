-- Seed: 1657269551920056738,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity bih is
  port (otslkr : buffer std_logic; g : linkage time; ulffemck : buffer bit_vector(4 to 3); lafamkaynd : in std_logic);
end bih;

architecture p of bih is
  
begin
  -- Single-driven assignments
  ulffemck <= ulffemck;
end p;

entity heaskngjnt is
  port (jwf : inout boolean; hlxlbtvj : in real);
end heaskngjnt;

library ieee;
use ieee.std_logic_1164.all;

architecture dqxds of heaskngjnt is
  signal b : std_logic;
  signal eipnpc : bit_vector(4 to 3);
  signal ntizrtu : time;
  signal ytwpixr : std_logic;
begin
  tinsdy : entity work.bih
    port map (otslkr => ytwpixr, g => ntizrtu, ulffemck => eipnpc, lafamkaynd => b);
  
  -- Single-driven assignments
  jwf <= TRUE;
  
  -- Multi-driven assignments
  b <= ytwpixr;
  ytwpixr <= '1';
  b <= '-';
end dqxds;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (eukqsiuraz : linkage std_logic_vector(1 downto 3); avade : buffer character; rpidguwjq : out bit_vector(3 downto 1); xtngk : in time);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture nfrgvnvgw of n is
  signal fhzmk : bit_vector(4 to 3);
  signal imptr : time;
  signal ktzcsx : std_logic;
  signal qqeygovfoo : std_logic;
  signal hpilbsk : bit_vector(4 to 3);
  signal s : time;
  signal fualjm : bit_vector(4 to 3);
  signal kihzmem : time;
  signal gxvdoqqku : std_logic;
begin
  xlznjtsdb : entity work.bih
    port map (otslkr => gxvdoqqku, g => kihzmem, ulffemck => fualjm, lafamkaynd => gxvdoqqku);
  ntmhfaegz : entity work.bih
    port map (otslkr => gxvdoqqku, g => s, ulffemck => hpilbsk, lafamkaynd => qqeygovfoo);
  pzoofztgf : entity work.bih
    port map (otslkr => ktzcsx, g => imptr, ulffemck => fhzmk, lafamkaynd => gxvdoqqku);
  
  -- Single-driven assignments
  avade <= 'x';
  rpidguwjq <= ('0', '0', '0');
  
  -- Multi-driven assignments
  gxvdoqqku <= gxvdoqqku;
end nfrgvnvgw;

entity q is
  port (lbf : inout boolean_vector(4 to 2); nu : linkage time);
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture zanb of q is
  signal dwakggvfh : time;
  signal wsynbqsw : bit_vector(3 downto 1);
  signal xbaiznj : character;
  signal eeiyr : std_logic_vector(1 downto 3);
  signal qjeaqaeas : boolean;
  signal pa : bit_vector(4 to 3);
  signal cqpntpq : std_logic;
  signal kzdpnhhvcz : real;
  signal zjjimjavm : boolean;
begin
  hmetwv : entity work.heaskngjnt
    port map (jwf => zjjimjavm, hlxlbtvj => kzdpnhhvcz);
  lqgrnjuzql : entity work.bih
    port map (otslkr => cqpntpq, g => nu, ulffemck => pa, lafamkaynd => cqpntpq);
  av : entity work.heaskngjnt
    port map (jwf => qjeaqaeas, hlxlbtvj => kzdpnhhvcz);
  wwhdbui : entity work.n
    port map (eukqsiuraz => eeiyr, avade => xbaiznj, rpidguwjq => wsynbqsw, xtngk => dwakggvfh);
  
  -- Single-driven assignments
  kzdpnhhvcz <= kzdpnhhvcz;
  dwakggvfh <= 02.3_4_1_2_2 ms;
  lbf <= lbf;
  
  -- Multi-driven assignments
  cqpntpq <= '1';
  cqpntpq <= 'Z';
end zanb;



-- Seed after: 12776753746803106275,8067602802092121131
