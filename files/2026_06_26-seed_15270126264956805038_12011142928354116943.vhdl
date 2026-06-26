-- Seed: 15270126264956805038,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity elqs is
  port ( cwgghitrrx : inout boolean_vector(2 to 4)
  ; xabg : out std_logic_vector(4 to 4)
  ; upwtf : linkage std_logic_vector(1 downto 0)
  ; psakdhtt : linkage time
  );
end elqs;

architecture bkwpocb of elqs is
  
begin
  -- Single-driven assignments
  cwgghitrrx <= (FALSE, TRUE, TRUE);
  
  -- Multi-driven assignments
  xabg <= (others => '1');
  xabg <= "L";
  xabg <= "X";
  xabg <= "W";
end bkwpocb;

entity nurgtg is
  port (hcsvcdjas : inout string(5 downto 3); azfoijo : buffer severity_level);
end nurgtg;

library ieee;
use ieee.std_logic_1164.all;

architecture iiacdsyo of nurgtg is
  signal n : time;
  signal tdnyk : std_logic_vector(1 downto 0);
  signal gdwbtjenj : std_logic_vector(4 to 4);
  signal oiyxvllhrd : boolean_vector(2 to 4);
  signal g : time;
  signal datleipl : boolean_vector(2 to 4);
  signal qjjsfrwxe : time;
  signal uzidvnnxur : std_logic_vector(1 downto 0);
  signal lrmf : std_logic_vector(4 to 4);
  signal blcxmbryf : boolean_vector(2 to 4);
begin
  arxg : entity work.elqs
    port map (cwgghitrrx => blcxmbryf, xabg => lrmf, upwtf => uzidvnnxur, psakdhtt => qjjsfrwxe);
  hinujokm : entity work.elqs
    port map (cwgghitrrx => datleipl, xabg => lrmf, upwtf => uzidvnnxur, psakdhtt => g);
  qdrpwjgtly : entity work.elqs
    port map (cwgghitrrx => oiyxvllhrd, xabg => gdwbtjenj, upwtf => tdnyk, psakdhtt => n);
end iiacdsyo;

library ieee;
use ieee.std_logic_1164.all;

entity bx is
  port (sa : in std_logic_vector(2 downto 4); lqtmevo : in integer);
end bx;

library ieee;
use ieee.std_logic_1164.all;

architecture umm of bx is
  signal acspkd : time;
  signal xtnpxdhid : std_logic_vector(1 downto 0);
  signal pwtptdte : std_logic_vector(4 to 4);
  signal jdcmw : boolean_vector(2 to 4);
begin
  ddqvkrdlts : entity work.elqs
    port map (cwgghitrrx => jdcmw, xabg => pwtptdte, upwtf => xtnpxdhid, psakdhtt => acspkd);
end umm;

entity nt is
  port (u : in integer; gc : buffer bit);
end nt;

library ieee;
use ieee.std_logic_1164.all;

architecture xps of nt is
  signal uqrmf : time;
  signal dohlqdev : boolean_vector(2 to 4);
  signal vfvycugdt : integer;
  signal yymcxamxg : std_logic_vector(2 downto 4);
  signal fbirexn : severity_level;
  signal dxuyab : string(5 downto 3);
  signal ddgknh : time;
  signal ldf : std_logic_vector(1 downto 0);
  signal etddj : std_logic_vector(4 to 4);
  signal topra : boolean_vector(2 to 4);
begin
  fkephmpnrt : entity work.elqs
    port map (cwgghitrrx => topra, xabg => etddj, upwtf => ldf, psakdhtt => ddgknh);
  hto : entity work.nurgtg
    port map (hcsvcdjas => dxuyab, azfoijo => fbirexn);
  ks : entity work.bx
    port map (sa => yymcxamxg, lqtmevo => vfvycugdt);
  sakpwncm : entity work.elqs
    port map (cwgghitrrx => dohlqdev, xabg => etddj, upwtf => ldf, psakdhtt => uqrmf);
  
  -- Single-driven assignments
  gc <= '0';
  vfvycugdt <= 2#10#;
  
  -- Multi-driven assignments
  etddj <= "-";
end xps;



-- Seed after: 7870603591770392043,12011142928354116943
