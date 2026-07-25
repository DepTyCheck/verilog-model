-- Seed: 7737270168884357359,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity blgmjryh is
  port (i : in boolean_vector(4 downto 0); ntuors : buffer std_logic; hopstjm : in boolean; l : buffer std_logic);
end blgmjryh;

architecture ubnjvown of blgmjryh is
  
begin
  -- Multi-driven assignments
  l <= l;
  ntuors <= 'X';
end ubnjvown;

entity ccdjguyjb is
  port (gf : in integer; chzsgt : out boolean; zfwmcf : buffer time);
end ccdjguyjb;

library ieee;
use ieee.std_logic_1164.all;

architecture nxbsrv of ccdjguyjb is
  signal gvk : boolean;
  signal krqeomxk : boolean_vector(4 downto 0);
  signal oesvbcpif : std_logic;
  signal hhscpyjnf : boolean;
  signal hn : boolean;
  signal qbpxkrtply : std_logic;
  signal uv : boolean_vector(4 downto 0);
begin
  znt : entity work.blgmjryh
    port map (i => uv, ntuors => qbpxkrtply, hopstjm => hn, l => qbpxkrtply);
  q : entity work.blgmjryh
    port map (i => uv, ntuors => qbpxkrtply, hopstjm => hhscpyjnf, l => qbpxkrtply);
  dohfqyb : entity work.blgmjryh
    port map (i => uv, ntuors => qbpxkrtply, hopstjm => chzsgt, l => oesvbcpif);
  rtb : entity work.blgmjryh
    port map (i => krqeomxk, ntuors => qbpxkrtply, hopstjm => gvk, l => oesvbcpif);
  
  -- Single-driven assignments
  hhscpyjnf <= gvk;
  gvk <= TRUE;
  hn <= chzsgt;
  
  -- Multi-driven assignments
  qbpxkrtply <= qbpxkrtply;
  qbpxkrtply <= qbpxkrtply;
  oesvbcpif <= 'L';
end nxbsrv;

library ieee;
use ieee.std_logic_1164.all;

entity wpixhyrpcl is
  port (lkkkwwjazc : in std_logic_vector(0 to 4); rawtxxfyus : linkage boolean);
end wpixhyrpcl;

library ieee;
use ieee.std_logic_1164.all;

architecture diyjgxv of wpixhyrpcl is
  signal wosp : std_logic;
  signal jtr : std_logic;
  signal ncsnuqcl : std_logic;
  signal gvnbyjjyca : std_logic;
  signal ka : time;
  signal xfnbeerjo : integer;
  signal eio : std_logic;
  signal hqlaqc : boolean;
  signal ntwbrpm : std_logic;
  signal qyidzcqpp : boolean_vector(4 downto 0);
begin
  qjjtl : entity work.blgmjryh
    port map (i => qyidzcqpp, ntuors => ntwbrpm, hopstjm => hqlaqc, l => eio);
  bzdmd : entity work.ccdjguyjb
    port map (gf => xfnbeerjo, chzsgt => hqlaqc, zfwmcf => ka);
  unjdfjwomd : entity work.blgmjryh
    port map (i => qyidzcqpp, ntuors => gvnbyjjyca, hopstjm => hqlaqc, l => ncsnuqcl);
  pkgto : entity work.blgmjryh
    port map (i => qyidzcqpp, ntuors => jtr, hopstjm => hqlaqc, l => wosp);
end diyjgxv;

entity ip is
  port (d : buffer integer; pg : inout boolean);
end ip;

library ieee;
use ieee.std_logic_1164.all;

architecture kycjolimm of ip is
  signal vzu : std_logic_vector(0 to 4);
begin
  t : entity work.wpixhyrpcl
    port map (lkkkwwjazc => vzu, rawtxxfyus => pg);
  
  -- Single-driven assignments
  d <= 2#0_0_0#;
  
  -- Multi-driven assignments
  vzu <= vzu;
  vzu <= vzu;
  vzu <= vzu;
  vzu <= vzu;
end kycjolimm;



-- Seed after: 4850957796278499588,5306691039457971049
