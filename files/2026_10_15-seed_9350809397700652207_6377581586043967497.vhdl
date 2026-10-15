-- Seed: 9350809397700652207,6377581586043967497

entity ckpx is
  port (obsz : in real; jjsmf : out boolean; ktquatixha : buffer time; x : out boolean_vector(4 downto 0));
end ckpx;

architecture ochagq of ckpx is
  
begin
  -- Single-driven assignments
  x <= (FALSE, TRUE, TRUE, TRUE, TRUE);
  jjsmf <= TRUE;
  ktquatixha <= 8#0_5.3_4# ms;
end ochagq;

entity regyrdggzb is
  port (uebwbi : inout real_vector(1 downto 3));
end regyrdggzb;

architecture gjahamvjg of regyrdggzb is
  signal tzluyeaxcq : boolean_vector(4 downto 0);
  signal ajpoq : time;
  signal bezove : boolean;
  signal zqaufoaepj : real;
  signal ytmdud : boolean_vector(4 downto 0);
  signal nlwuljdod : time;
  signal oxxvwoi : boolean;
  signal mue : real;
begin
  vvggtbqk : entity work.ckpx
    port map (obsz => mue, jjsmf => oxxvwoi, ktquatixha => nlwuljdod, x => ytmdud);
  h : entity work.ckpx
    port map (obsz => zqaufoaepj, jjsmf => bezove, ktquatixha => ajpoq, x => tzluyeaxcq);
  
  -- Single-driven assignments
  uebwbi <= uebwbi;
  zqaufoaepj <= 444.0;
  mue <= 2#0_0_1_0_0.1_0#;
end gjahamvjg;

entity sichhezzqn is
  port (klb : in boolean);
end sichhezzqn;

architecture ucsagjdbz of sichhezzqn is
  signal lufcwnyf : real_vector(1 downto 3);
  signal yzohxbfasa : boolean_vector(4 downto 0);
  signal i : time;
  signal rxh : boolean;
  signal snwdpymy : real;
begin
  urcsyyitp : entity work.ckpx
    port map (obsz => snwdpymy, jjsmf => rxh, ktquatixha => i, x => yzohxbfasa);
  vkp : entity work.regyrdggzb
    port map (uebwbi => lufcwnyf);
end ucsagjdbz;

library ieee;
use ieee.std_logic_1164.all;

entity pryglq is
  port (rmgxradxu : buffer severity_level; jtslyxzyov : out std_logic);
end pryglq;

architecture vkyo of pryglq is
  signal es : real_vector(1 downto 3);
  signal hbv : boolean_vector(4 downto 0);
  signal avsftghsws : time;
  signal beofvnqz : real;
  signal rnpbqghfro : boolean;
begin
  pywmyu : entity work.sichhezzqn
    port map (klb => rnpbqghfro);
  xndksmm : entity work.sichhezzqn
    port map (klb => rnpbqghfro);
  bopjacu : entity work.ckpx
    port map (obsz => beofvnqz, jjsmf => rnpbqghfro, ktquatixha => avsftghsws, x => hbv);
  iaqtwnqho : entity work.regyrdggzb
    port map (uebwbi => es);
  
  -- Multi-driven assignments
  jtslyxzyov <= jtslyxzyov;
  jtslyxzyov <= jtslyxzyov;
  jtslyxzyov <= '-';
  jtslyxzyov <= jtslyxzyov;
end vkyo;



-- Seed after: 10143198834726029993,6377581586043967497
