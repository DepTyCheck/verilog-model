-- Seed: 14334821724742261919,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity arufbvec is
  port (jljifw : inout real; fqlse : out std_logic; vbbmkwxni : buffer std_logic; uoekelvaqc : in time);
end arufbvec;

architecture lqglykmg of arufbvec is
  
begin
  -- Single-driven assignments
  jljifw <= 2.04;
  
  -- Multi-driven assignments
  fqlse <= '1';
end lqglykmg;

library ieee;
use ieee.std_logic_1164.all;

entity mkqtpfd is
  port (glzsgyppo : out bit; iarttl : buffer std_logic);
end mkqtpfd;

library ieee;
use ieee.std_logic_1164.all;

architecture dhewjixn of mkqtpfd is
  signal rxpbxmwvwf : time;
  signal ibbhy : std_logic;
  signal bosacoe : std_logic;
  signal hm : real;
begin
  rc : entity work.arufbvec
    port map (jljifw => hm, fqlse => bosacoe, vbbmkwxni => ibbhy, uoekelvaqc => rxpbxmwvwf);
  
  -- Single-driven assignments
  glzsgyppo <= '0';
  rxpbxmwvwf <= 0 hr;
  
  -- Multi-driven assignments
  bosacoe <= iarttl;
  ibbhy <= iarttl;
end dhewjixn;

entity avbiqzbl is
  port (hgmcxm : linkage time_vector(2 downto 2));
end avbiqzbl;

library ieee;
use ieee.std_logic_1164.all;

architecture cpq of avbiqzbl is
  signal cxommuh : time;
  signal mjpqdsvoy : std_logic;
  signal ucuptqyu : real;
  signal fyoje : real;
  signal hrwjxxxnws : time;
  signal w : std_logic;
  signal shguunw : real;
begin
  ore : entity work.arufbvec
    port map (jljifw => shguunw, fqlse => w, vbbmkwxni => w, uoekelvaqc => hrwjxxxnws);
  ovfhtzw : entity work.arufbvec
    port map (jljifw => fyoje, fqlse => w, vbbmkwxni => w, uoekelvaqc => hrwjxxxnws);
  c : entity work.arufbvec
    port map (jljifw => ucuptqyu, fqlse => w, vbbmkwxni => mjpqdsvoy, uoekelvaqc => cxommuh);
  
  -- Multi-driven assignments
  w <= mjpqdsvoy;
  mjpqdsvoy <= '-';
  w <= mjpqdsvoy;
end cpq;

entity ciievkbuz is
  port (ivp : linkage time);
end ciievkbuz;

library ieee;
use ieee.std_logic_1164.all;

architecture vjkal of ciievkbuz is
  signal rgwqtt : bit;
  signal qzypzw : time;
  signal zdqsaol : std_logic;
  signal ysudwtjny : std_logic;
  signal twjc : real;
  signal isdqdzz : time_vector(2 downto 2);
begin
  niia : entity work.avbiqzbl
    port map (hgmcxm => isdqdzz);
  kek : entity work.arufbvec
    port map (jljifw => twjc, fqlse => ysudwtjny, vbbmkwxni => zdqsaol, uoekelvaqc => qzypzw);
  ysdilna : entity work.mkqtpfd
    port map (glzsgyppo => rgwqtt, iarttl => zdqsaol);
  
  -- Single-driven assignments
  qzypzw <= 8#662.04# us;
  
  -- Multi-driven assignments
  ysudwtjny <= ysudwtjny;
  ysudwtjny <= ysudwtjny;
  zdqsaol <= 'H';
  zdqsaol <= '1';
end vjkal;



-- Seed after: 2883545184677414222,4080032123900078489
