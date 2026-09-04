-- Seed: 6438933729032725356,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity ihicrj is
  port (hdcoul : in real; u : inout severity_level; gvlmce : buffer real; ibbr : inout std_logic_vector(0 to 3));
end ihicrj;

architecture om of ihicrj is
  
begin
  -- Multi-driven assignments
  ibbr <= ('W', 'X', '1', '0');
  ibbr <= ibbr;
end om;

entity ivda is
  port (gwb : linkage real_vector(2 downto 1); sfqvbxbxub : buffer character; mzhysiju : inout bit; g : in real);
end ivda;

library ieee;
use ieee.std_logic_1164.all;

architecture rukdhqa of ivda is
  signal bcovd : std_logic_vector(0 to 3);
  signal iaugajtiwf : severity_level;
  signal upsfht : severity_level;
  signal tubpjowz : real;
  signal mnmaqz : std_logic_vector(0 to 3);
  signal xnwrgf : real;
  signal ewanhi : severity_level;
  signal o : real;
begin
  rapbap : entity work.ihicrj
    port map (hdcoul => o, u => ewanhi, gvlmce => xnwrgf, ibbr => mnmaqz);
  tpwlokb : entity work.ihicrj
    port map (hdcoul => tubpjowz, u => upsfht, gvlmce => tubpjowz, ibbr => mnmaqz);
  sgxfymp : entity work.ihicrj
    port map (hdcoul => tubpjowz, u => iaugajtiwf, gvlmce => o, ibbr => bcovd);
  
  -- Single-driven assignments
  mzhysiju <= '1';
  
  -- Multi-driven assignments
  bcovd <= mnmaqz;
end rukdhqa;

entity o is
  port (ofaijdl : inout integer_vector(1 downto 1); qqddxq : linkage integer_vector(1 downto 4));
end o;

architecture dbh of o is
  signal ltscnbqhk : real;
  signal lwk : bit;
  signal bzz : character;
  signal slil : real_vector(2 downto 1);
begin
  c : entity work.ivda
    port map (gwb => slil, sfqvbxbxub => bzz, mzhysiju => lwk, g => ltscnbqhk);
end dbh;



-- Seed after: 17803273027140286841,4404421571376382767
