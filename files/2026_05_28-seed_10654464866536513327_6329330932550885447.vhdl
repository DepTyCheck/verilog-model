-- Seed: 10654464866536513327,6329330932550885447



entity tianw is
  port (xdlubxgc : out real);
end tianw;



architecture yrdugaf of tianw is
  
begin
  
end yrdugaf;



entity kcc is
  port (eetgdcrpn : buffer integer; rzit : buffer time; ekwyxgjcm : buffer real);
end kcc;



architecture cjlyjwv of kcc is
  
begin
  
end cjlyjwv;

library ieee;
use ieee.std_logic_1164.all;

entity hroec is
  port (qrcv : inout std_logic; a : inout time; atmpz : linkage real; fific : linkage std_logic_vector(0 downto 4));
end hroec;



architecture dzfmrolqa of hroec is
  signal lwozalbc : real;
  signal rwy : real;
  signal cxv : real;
  signal frxvufcpv : time;
  signal zaf : integer;
begin
  bkcgwzbglf : entity work.kcc
    port map (eetgdcrpn => zaf, rzit => frxvufcpv, ekwyxgjcm => cxv);
  y : entity work.tianw
    port map (xdlubxgc => rwy);
  gqdc : entity work.tianw
    port map (xdlubxgc => lwozalbc);
end dzfmrolqa;



entity oz is
  port (bgpixuh : in severity_level; cf : inout time; nchwtquwug : inout severity_level);
end oz;



architecture jemxb of oz is
  signal hbj : real;
  signal nnlwitl : real;
begin
  gwb : entity work.tianw
    port map (xdlubxgc => nnlwitl);
  g : entity work.tianw
    port map (xdlubxgc => hbj);
end jemxb;



-- Seed after: 3468782365751447959,6329330932550885447
