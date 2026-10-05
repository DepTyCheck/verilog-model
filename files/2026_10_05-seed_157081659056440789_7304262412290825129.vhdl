-- Seed: 157081659056440789,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity qnhpnogc is
  port (jocbpf : inout time; gyytoj : inout std_logic_vector(4 downto 3));
end qnhpnogc;

architecture qleynvtluo of qnhpnogc is
  
begin
  -- Single-driven assignments
  jocbpf <= 16#E4F69.9# ms;
  
  -- Multi-driven assignments
  gyytoj <= ('-', 'H');
  gyytoj <= "-H";
  gyytoj <= "U1";
end qleynvtluo;

entity npuy is
  port (xhkbv : in integer);
end npuy;

library ieee;
use ieee.std_logic_1164.all;

architecture vrds of npuy is
  signal uqudizt : time;
  signal tk : time;
  signal i : std_logic_vector(4 downto 3);
  signal lblz : time;
begin
  wuc : entity work.qnhpnogc
    port map (jocbpf => lblz, gyytoj => i);
  jmybi : entity work.qnhpnogc
    port map (jocbpf => tk, gyytoj => i);
  jpw : entity work.qnhpnogc
    port map (jocbpf => uqudizt, gyytoj => i);
  
  -- Multi-driven assignments
  i <= ('-', '1');
  i <= ('0', 'U');
  i <= ('1', '1');
end vrds;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (ydrprmt : in std_logic);
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture gwxckfcs of u is
  signal awi : std_logic_vector(4 downto 3);
  signal rzccwyvv : time;
  signal assxpd : std_logic_vector(4 downto 3);
  signal mkrx : time;
  signal nmxgfodkg : integer;
  signal hzkjctle : integer;
begin
  kortusukuv : entity work.npuy
    port map (xhkbv => hzkjctle);
  ljfrum : entity work.npuy
    port map (xhkbv => nmxgfodkg);
  qrmk : entity work.qnhpnogc
    port map (jocbpf => mkrx, gyytoj => assxpd);
  ect : entity work.qnhpnogc
    port map (jocbpf => rzccwyvv, gyytoj => awi);
  
  -- Single-driven assignments
  hzkjctle <= hzkjctle;
  nmxgfodkg <= hzkjctle;
end gwxckfcs;



-- Seed after: 14744542666828087531,7304262412290825129
