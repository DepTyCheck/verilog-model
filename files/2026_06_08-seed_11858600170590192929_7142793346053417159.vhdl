-- Seed: 11858600170590192929,7142793346053417159

library ieee;
use ieee.std_logic_1164.all;

entity bgrreyaf is
  port (b : linkage std_logic_vector(1 downto 4); xzfj : in boolean; bzhcdsbyv : inout bit_vector(2 to 4));
end bgrreyaf;



architecture fi of bgrreyaf is
  
begin
  
end fi;

library ieee;
use ieee.std_logic_1164.all;

entity bctgzngul is
  port (mpjexw : inout std_logic; zcchte : buffer std_logic; xsvp : buffer time_vector(2 to 0));
end bctgzngul;

library ieee;
use ieee.std_logic_1164.all;

architecture pkiojdnkbc of bctgzngul is
  signal cacgy : bit_vector(2 to 4);
  signal w : boolean;
  signal ro : bit_vector(2 to 4);
  signal uop : boolean;
  signal qkq : std_logic_vector(1 downto 4);
begin
  ffooviay : entity work.bgrreyaf
    port map (b => qkq, xzfj => uop, bzhcdsbyv => ro);
  ozmblu : entity work.bgrreyaf
    port map (b => qkq, xzfj => w, bzhcdsbyv => cacgy);
end pkiojdnkbc;



entity cim is
  port (zhwybnt : out real; xmebzedwy : out character);
end cim;

library ieee;
use ieee.std_logic_1164.all;

architecture yylfbcfpv of cim is
  signal ol : time_vector(2 to 0);
  signal jgtqxtakv : std_logic;
  signal aunngi : bit_vector(2 to 4);
  signal jzblgflb : boolean;
  signal pb : std_logic_vector(1 downto 4);
  signal oufvvs : time_vector(2 to 0);
  signal cq : std_logic;
begin
  lnzgum : entity work.bctgzngul
    port map (mpjexw => cq, zcchte => cq, xsvp => oufvvs);
  mivfwfrbts : entity work.bgrreyaf
    port map (b => pb, xzfj => jzblgflb, bzhcdsbyv => aunngi);
  xuu : entity work.bctgzngul
    port map (mpjexw => jgtqxtakv, zcchte => cq, xsvp => ol);
end yylfbcfpv;



entity nh is
  port (lnv : inout bit_vector(0 to 2));
end nh;

library ieee;
use ieee.std_logic_1164.all;

architecture y of nh is
  signal ivw : boolean;
  signal utqlbu : std_logic_vector(1 downto 4);
  signal ibs : character;
  signal tf : real;
  signal nnzkzcwas : time_vector(2 to 0);
  signal ehdnvaxvw : std_logic;
begin
  rfgwflemrp : entity work.bctgzngul
    port map (mpjexw => ehdnvaxvw, zcchte => ehdnvaxvw, xsvp => nnzkzcwas);
  dgipphu : entity work.cim
    port map (zhwybnt => tf, xmebzedwy => ibs);
  inc : entity work.bgrreyaf
    port map (b => utqlbu, xzfj => ivw, bzhcdsbyv => lnv);
end y;



-- Seed after: 10222055941056650785,7142793346053417159
