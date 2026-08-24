-- Seed: 12331845821733402229,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity aozq is
  port (les : out boolean; pgrqq : inout std_logic_vector(2 downto 4); ouc : buffer real);
end aozq;

architecture xuky of aozq is
  
begin
  -- Single-driven assignments
  ouc <= 4_1_3_3_2.011;
  les <= les;
  
  -- Multi-driven assignments
  pgrqq <= "";
  pgrqq <= pgrqq;
  pgrqq <= pgrqq;
  pgrqq <= (others => '0');
end xuky;

entity mpdwata is
  port (xxxsy : buffer real; xr : out real);
end mpdwata;

library ieee;
use ieee.std_logic_1164.all;

architecture cxidout of mpdwata is
  signal hzyknxu : real;
  signal gkovd : std_logic_vector(2 downto 4);
  signal wnozynl : boolean;
  signal lmjjtbjcqt : real;
  signal vpqsgtoqq : std_logic_vector(2 downto 4);
  signal nl : boolean;
  signal czoai : real;
  signal ok : boolean;
  signal ovcx : real;
  signal vxbkdetg : std_logic_vector(2 downto 4);
  signal vciuxgmjuo : boolean;
begin
  sbqmju : entity work.aozq
    port map (les => vciuxgmjuo, pgrqq => vxbkdetg, ouc => ovcx);
  jde : entity work.aozq
    port map (les => ok, pgrqq => vxbkdetg, ouc => czoai);
  yxbvuo : entity work.aozq
    port map (les => nl, pgrqq => vpqsgtoqq, ouc => lmjjtbjcqt);
  sdkghpyztp : entity work.aozq
    port map (les => wnozynl, pgrqq => gkovd, ouc => hzyknxu);
  
  -- Single-driven assignments
  xxxsy <= xr;
  
  -- Multi-driven assignments
  gkovd <= "";
  vxbkdetg <= (others => '0');
  vpqsgtoqq <= vxbkdetg;
end cxidout;

library ieee;
use ieee.std_logic_1164.all;

entity hngl is
  port (qunjxkx : out std_logic_vector(2 to 2));
end hngl;

library ieee;
use ieee.std_logic_1164.all;

architecture ievnotrqd of hngl is
  signal xgwyz : real;
  signal hdo : std_logic_vector(2 downto 4);
  signal tduhayo : boolean;
  signal ekkinafd : real;
  signal fbav : std_logic_vector(2 downto 4);
  signal fkaisahoqi : boolean;
  signal z : real;
  signal tmbgzei : real;
begin
  wicgnr : entity work.mpdwata
    port map (xxxsy => tmbgzei, xr => z);
  rithfqnwe : entity work.aozq
    port map (les => fkaisahoqi, pgrqq => fbav, ouc => ekkinafd);
  csmy : entity work.aozq
    port map (les => tduhayo, pgrqq => hdo, ouc => xgwyz);
  
  -- Multi-driven assignments
  fbav <= (others => '0');
end ievnotrqd;

library ieee;
use ieee.std_logic_1164.all;

entity ljxcin is
  port (mjtob : in bit; cjjtd : buffer std_logic_vector(2 to 3); y : in time; mtd : out real);
end ljxcin;

architecture gi of ljxcin is
  signal mmzquzth : real;
begin
  eattsyb : entity work.mpdwata
    port map (xxxsy => mmzquzth, xr => mtd);
end gi;



-- Seed after: 2361493276895796769,16159265764638711791
