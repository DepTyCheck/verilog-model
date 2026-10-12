-- Seed: 9676877581006746378,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity oefftmo is
  port ( hvpckhwgtl : inout std_logic_vector(3 downto 1)
  ; zy : linkage std_logic_vector(3 downto 4)
  ; grkffvtvnr : linkage severity_level
  ; zzfrm : in bit_vector(0 to 2)
  );
end oefftmo;

architecture pf of oefftmo is
  
begin
  -- Multi-driven assignments
  hvpckhwgtl <= hvpckhwgtl;
end pf;

library ieee;
use ieee.std_logic_1164.all;

entity rdlcb is
  port (gtxwxxgaom : in std_logic; uapaewdu : out time);
end rdlcb;

library ieee;
use ieee.std_logic_1164.all;

architecture oli of rdlcb is
  signal hdipy : bit_vector(0 to 2);
  signal wup : severity_level;
  signal cdlyofey : std_logic_vector(3 downto 4);
  signal bxqeeuefy : std_logic_vector(3 downto 1);
begin
  k : entity work.oefftmo
    port map (hvpckhwgtl => bxqeeuefy, zy => cdlyofey, grkffvtvnr => wup, zzfrm => hdipy);
  
  -- Single-driven assignments
  hdipy <= ('1', '0', '0');
  
  -- Multi-driven assignments
  bxqeeuefy <= ('W', 'L', 'H');
  cdlyofey <= "";
  bxqeeuefy <= bxqeeuefy;
end oli;

library ieee;
use ieee.std_logic_1164.all;

entity fqg is
  port (gegk : linkage std_logic_vector(2 to 3); brxvaygygk : buffer time; attb : in boolean_vector(3 to 1); sjsiikimt : out std_logic);
end fqg;

library ieee;
use ieee.std_logic_1164.all;

architecture eutxo of fqg is
  signal tjzgebmq : severity_level;
  signal nmt : std_logic_vector(3 downto 4);
  signal iqow : std_logic_vector(3 downto 1);
  signal fmwssm : severity_level;
  signal ycqbmnw : std_logic_vector(3 downto 1);
  signal fiwddhas : bit_vector(0 to 2);
  signal om : severity_level;
  signal f : std_logic_vector(3 downto 4);
  signal vmxhuuuepl : std_logic_vector(3 downto 1);
begin
  jv : entity work.oefftmo
    port map (hvpckhwgtl => vmxhuuuepl, zy => f, grkffvtvnr => om, zzfrm => fiwddhas);
  ygyveoqm : entity work.oefftmo
    port map (hvpckhwgtl => ycqbmnw, zy => f, grkffvtvnr => fmwssm, zzfrm => fiwddhas);
  l : entity work.oefftmo
    port map (hvpckhwgtl => iqow, zy => nmt, grkffvtvnr => tjzgebmq, zzfrm => fiwddhas);
  vdpsahk : entity work.rdlcb
    port map (gtxwxxgaom => sjsiikimt, uapaewdu => brxvaygygk);
  
  -- Single-driven assignments
  fiwddhas <= ('0', '0', '0');
  
  -- Multi-driven assignments
  sjsiikimt <= sjsiikimt;
  nmt <= (others => '0');
  f <= f;
  sjsiikimt <= sjsiikimt;
end eutxo;

entity g is
  port (lnsp : linkage integer);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture ey of g is
  signal lpubgb : bit_vector(0 to 2);
  signal smhddk : severity_level;
  signal eitp : std_logic_vector(3 downto 4);
  signal jkrnunjsx : std_logic_vector(3 downto 1);
  signal ukrano : severity_level;
  signal sow : std_logic_vector(3 downto 4);
  signal fjsp : std_logic_vector(3 downto 1);
  signal yiftdot : bit_vector(0 to 2);
  signal avuov : severity_level;
  signal w : std_logic_vector(3 downto 4);
  signal fgu : std_logic_vector(3 downto 1);
begin
  exn : entity work.oefftmo
    port map (hvpckhwgtl => fgu, zy => w, grkffvtvnr => avuov, zzfrm => yiftdot);
  mwrtwey : entity work.oefftmo
    port map (hvpckhwgtl => fjsp, zy => sow, grkffvtvnr => ukrano, zzfrm => yiftdot);
  efsk : entity work.oefftmo
    port map (hvpckhwgtl => jkrnunjsx, zy => eitp, grkffvtvnr => smhddk, zzfrm => lpubgb);
  
  -- Single-driven assignments
  lpubgb <= yiftdot;
  yiftdot <= yiftdot;
  
  -- Multi-driven assignments
  w <= eitp;
  sow <= "";
  jkrnunjsx <= fgu;
  fgu <= "HZL";
end ey;



-- Seed after: 15613280396452104535,3426943171337919827
