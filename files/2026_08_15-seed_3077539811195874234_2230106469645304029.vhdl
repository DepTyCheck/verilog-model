-- Seed: 3077539811195874234,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity cxaf is
  port (hei : out std_logic_vector(4 downto 3));
end cxaf;

architecture f of cxaf is
  
begin
  -- Multi-driven assignments
  hei <= ('U', 'U');
  hei <= hei;
  hei <= hei;
  hei <= hei;
end f;

entity cdqi is
  port (vgnoxaf : linkage severity_level);
end cdqi;

library ieee;
use ieee.std_logic_1164.all;

architecture neb of cdqi is
  signal ywesjavd : std_logic_vector(4 downto 3);
  signal nmws : std_logic_vector(4 downto 3);
  signal wx : std_logic_vector(4 downto 3);
begin
  vgvrgr : entity work.cxaf
    port map (hei => wx);
  va : entity work.cxaf
    port map (hei => nmws);
  eck : entity work.cxaf
    port map (hei => ywesjavd);
  ofcyfgc : entity work.cxaf
    port map (hei => ywesjavd);
  
  -- Multi-driven assignments
  ywesjavd <= wx;
  wx <= nmws;
  nmws <= wx;
end neb;

entity wr is
  port (k : in real_vector(3 downto 0));
end wr;

library ieee;
use ieee.std_logic_1164.all;

architecture dhbvyiq of wr is
  signal nrbtybqnw : severity_level;
  signal zpzqzwyhgn : std_logic_vector(4 downto 3);
begin
  i : entity work.cxaf
    port map (hei => zpzqzwyhgn);
  topeapa : entity work.cdqi
    port map (vgnoxaf => nrbtybqnw);
  
  -- Multi-driven assignments
  zpzqzwyhgn <= "-X";
  zpzqzwyhgn <= zpzqzwyhgn;
  zpzqzwyhgn <= ('X', 'Z');
  zpzqzwyhgn <= "1Z";
end dhbvyiq;



-- Seed after: 13707867824392517879,2230106469645304029
