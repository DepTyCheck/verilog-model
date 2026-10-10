-- Seed: 847803649284283729,511364357853360275

entity fkwktbusw is
  port (lfblp : buffer integer; xlhq : out real; eddr : linkage bit; tliev : inout time_vector(1 downto 3));
end fkwktbusw;

architecture g of fkwktbusw is
  
begin
  -- Single-driven assignments
  tliev <= tliev;
  xlhq <= 33.1132;
end g;

entity wqxyct is
  port (bhnclgdo : linkage time_vector(0 to 3); lwdembpsfq : out real);
end wqxyct;

architecture rxncemdt of wqxyct is
  signal zzztaw : time_vector(1 downto 3);
  signal pft : bit;
  signal fu : integer;
  signal cfqt : time_vector(1 downto 3);
  signal eesrvfzqc : bit;
  signal flfelt : real;
  signal chfrchx : integer;
  signal glmntu : time_vector(1 downto 3);
  signal gzrryrip : bit;
  signal ppjyufs : real;
  signal gvaezgxi : integer;
begin
  frkpdr : entity work.fkwktbusw
    port map (lfblp => gvaezgxi, xlhq => ppjyufs, eddr => gzrryrip, tliev => glmntu);
  k : entity work.fkwktbusw
    port map (lfblp => chfrchx, xlhq => flfelt, eddr => eesrvfzqc, tliev => cfqt);
  hb : entity work.fkwktbusw
    port map (lfblp => fu, xlhq => lwdembpsfq, eddr => pft, tliev => zzztaw);
end rxncemdt;

library ieee;
use ieee.std_logic_1164.all;

entity whjtatyvnt is
  port (dhsoonkct : inout std_logic);
end whjtatyvnt;

architecture qepbmkv of whjtatyvnt is
  signal orjepr : time_vector(1 downto 3);
  signal wzmjhpinjv : bit;
  signal dqqxlg : real;
  signal gkow : integer;
begin
  vq : entity work.fkwktbusw
    port map (lfblp => gkow, xlhq => dqqxlg, eddr => wzmjhpinjv, tliev => orjepr);
  
  -- Multi-driven assignments
  dhsoonkct <= 'U';
end qepbmkv;



-- Seed after: 5798662310978279458,511364357853360275
