-- Seed: 2243566282175149286,17924494779688682807

entity boxwaw is
  port (qf : in boolean; qktcli : inout string(4 downto 3));
end boxwaw;

architecture stx of boxwaw is
  
begin
  
end stx;

entity rdybxj is
  port (gbxro : buffer integer);
end rdybxj;

architecture dfstevxgfn of rdybxj is
  signal mxhq : string(4 downto 3);
  signal kgqtmdx : boolean;
  signal thwrhhjfp : string(4 downto 3);
  signal bdwqoak : string(4 downto 3);
  signal rknpsl : string(4 downto 3);
  signal ohecrv : boolean;
begin
  rj : entity work.boxwaw
    port map (qf => ohecrv, qktcli => rknpsl);
  zxoloq : entity work.boxwaw
    port map (qf => ohecrv, qktcli => bdwqoak);
  pcqpz : entity work.boxwaw
    port map (qf => ohecrv, qktcli => thwrhhjfp);
  avacukos : entity work.boxwaw
    port map (qf => kgqtmdx, qktcli => mxhq);
  
  -- Single-driven assignments
  gbxro <= 16#A_7_C#;
  kgqtmdx <= TRUE;
  ohecrv <= FALSE;
end dfstevxgfn;

library ieee;
use ieee.std_logic_1164.all;

entity ojdzdzmq is
  port (o : inout integer; bvnzfbi : linkage severity_level; nolbb : linkage std_logic_vector(0 to 0));
end ojdzdzmq;

architecture xqk of ojdzdzmq is
  signal wmuehybsu : integer;
  signal bknno : string(4 downto 3);
  signal rb : boolean;
  signal psj : string(4 downto 3);
  signal h : boolean;
begin
  pmq : entity work.boxwaw
    port map (qf => h, qktcli => psj);
  tef : entity work.boxwaw
    port map (qf => rb, qktcli => bknno);
  rtyjci : entity work.rdybxj
    port map (gbxro => wmuehybsu);
  
  -- Single-driven assignments
  o <= 02;
end xqk;

library ieee;
use ieee.std_logic_1164.all;

entity kvaso is
  port (ygpgdv : buffer std_logic; zkt : linkage time; jl : out real_vector(4 to 1));
end kvaso;

library ieee;
use ieee.std_logic_1164.all;

architecture fkcuerb of kvaso is
  signal kcenwfkl : std_logic_vector(0 to 0);
  signal yhehwcd : severity_level;
  signal lbwg : integer;
  signal dekolzmapc : string(4 downto 3);
  signal t : boolean;
begin
  hlhujkxfa : entity work.boxwaw
    port map (qf => t, qktcli => dekolzmapc);
  iyfop : entity work.ojdzdzmq
    port map (o => lbwg, bvnzfbi => yhehwcd, nolbb => kcenwfkl);
  
  -- Single-driven assignments
  jl <= (others => 0.0);
  t <= FALSE;
  
  -- Multi-driven assignments
  ygpgdv <= 'W';
end fkcuerb;



-- Seed after: 12149200692204411944,17924494779688682807
