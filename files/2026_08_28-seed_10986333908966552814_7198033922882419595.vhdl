-- Seed: 10986333908966552814,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity acrq is
  port (egxumosodb : linkage std_logic_vector(4 to 3); nyqkilbpi : in time; z : in time; bacs : inout time);
end acrq;

architecture ewj of acrq is
  
begin
  
end ewj;

library ieee;
use ieee.std_logic_1164.all;

entity penmrm is
  port (crveebzx : in integer; wdwst : in std_logic_vector(3 downto 3));
end penmrm;

library ieee;
use ieee.std_logic_1164.all;

architecture gzcrbhmxgr of penmrm is
  signal awl : time;
  signal ruqfqkbm : time;
  signal jnfl : std_logic_vector(4 to 3);
  signal lcilcewgqe : time;
  signal pdqcjrxocz : time;
  signal zspmyefumt : time;
  signal plfx : time;
  signal lzot : time;
  signal r : std_logic_vector(4 to 3);
  signal w : time;
  signal moxms : std_logic_vector(4 to 3);
begin
  qniehvklr : entity work.acrq
    port map (egxumosodb => moxms, nyqkilbpi => w, z => w, bacs => w);
  boynfz : entity work.acrq
    port map (egxumosodb => r, nyqkilbpi => lzot, z => lzot, bacs => plfx);
  dkayq : entity work.acrq
    port map (egxumosodb => moxms, nyqkilbpi => zspmyefumt, z => pdqcjrxocz, bacs => lcilcewgqe);
  k : entity work.acrq
    port map (egxumosodb => jnfl, nyqkilbpi => ruqfqkbm, z => awl, bacs => lzot);
  
  -- Multi-driven assignments
  jnfl <= moxms;
  moxms <= "";
  moxms <= "";
  moxms <= "";
end gzcrbhmxgr;

library ieee;
use ieee.std_logic_1164.all;

entity widnr is
  port (sdu : in integer_vector(0 to 2); aqm : in std_logic_vector(2 to 4));
end widnr;

library ieee;
use ieee.std_logic_1164.all;

architecture fyrbfuzv of widnr is
  signal cm : time;
  signal vpfkkqx : time;
  signal gel : time;
  signal pd : std_logic_vector(4 to 3);
  signal ywdw : std_logic_vector(3 downto 3);
  signal cahp : integer;
begin
  pg : entity work.penmrm
    port map (crveebzx => cahp, wdwst => ywdw);
  ryklbe : entity work.acrq
    port map (egxumosodb => pd, nyqkilbpi => gel, z => gel, bacs => vpfkkqx);
  eiwnmu : entity work.acrq
    port map (egxumosodb => pd, nyqkilbpi => cm, z => vpfkkqx, bacs => gel);
  
  -- Single-driven assignments
  cahp <= 404;
  cm <= vpfkkqx;
  
  -- Multi-driven assignments
  ywdw <= ywdw;
  pd <= pd;
  ywdw <= ywdw;
  ywdw <= ywdw;
end fyrbfuzv;

library ieee;
use ieee.std_logic_1164.all;

entity lginspbgmx is
  port (aos : buffer time; rs : out bit_vector(3 downto 4); qaotv : inout std_logic_vector(3 downto 2); bcvimvm : in real);
end lginspbgmx;

library ieee;
use ieee.std_logic_1164.all;

architecture oaupzk of lginspbgmx is
  signal mdfksvmip : std_logic_vector(3 downto 3);
  signal zhub : integer;
  signal csqxnqwtbg : std_logic_vector(2 to 4);
  signal b : integer_vector(0 to 2);
begin
  vzkuixcdkg : entity work.widnr
    port map (sdu => b, aqm => csqxnqwtbg);
  jkuekwp : entity work.penmrm
    port map (crveebzx => zhub, wdwst => mdfksvmip);
  
  -- Single-driven assignments
  rs <= (others => '0');
  zhub <= zhub;
  aos <= 2#0_0_1_1# us;
  
  -- Multi-driven assignments
  mdfksvmip <= mdfksvmip;
  mdfksvmip <= mdfksvmip;
  mdfksvmip <= "U";
  qaotv <= qaotv;
end oaupzk;



-- Seed after: 11532479141372907477,7198033922882419595
