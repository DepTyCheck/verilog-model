-- Seed: 17622690000951528191,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity nvo is
  port (nq : inout integer_vector(3 to 3); ldjd : out std_logic);
end nvo;

architecture zlvwhy of nvo is
  
begin
  -- Single-driven assignments
  nq <= (others => 4_1_2_2_0);
  
  -- Multi-driven assignments
  ldjd <= '1';
  ldjd <= '0';
end zlvwhy;

entity kuzlwysw is
  port (wyh : buffer time; xdaphr : inout severity_level; phbcbm : in time);
end kuzlwysw;

library ieee;
use ieee.std_logic_1164.all;

architecture vjjqlgg of kuzlwysw is
  signal efrq : integer_vector(3 to 3);
  signal sfewwa : std_logic;
  signal xoeqggfs : integer_vector(3 to 3);
  signal ji : integer_vector(3 to 3);
  signal dconcyhvkm : std_logic;
  signal oqgjfm : integer_vector(3 to 3);
begin
  ptsnxeoup : entity work.nvo
    port map (nq => oqgjfm, ldjd => dconcyhvkm);
  crel : entity work.nvo
    port map (nq => ji, ldjd => dconcyhvkm);
  gtapl : entity work.nvo
    port map (nq => xoeqggfs, ldjd => sfewwa);
  rbhuiueulv : entity work.nvo
    port map (nq => efrq, ldjd => dconcyhvkm);
end vjjqlgg;

entity bwhff is
  port (mevt : out bit);
end bwhff;

library ieee;
use ieee.std_logic_1164.all;

architecture kvybabwo of bwhff is
  signal rycehvkztf : integer_vector(3 to 3);
  signal zhfr : std_logic;
  signal atupg : integer_vector(3 to 3);
  signal wychmpudf : std_logic;
  signal fdafmp : integer_vector(3 to 3);
  signal yhsozfhbxy : time;
  signal awjfws : severity_level;
  signal epg : time;
begin
  dbvgatt : entity work.kuzlwysw
    port map (wyh => epg, xdaphr => awjfws, phbcbm => yhsozfhbxy);
  plhgdepdlh : entity work.nvo
    port map (nq => fdafmp, ldjd => wychmpudf);
  f : entity work.nvo
    port map (nq => atupg, ldjd => zhfr);
  cybtk : entity work.nvo
    port map (nq => rycehvkztf, ldjd => wychmpudf);
  
  -- Single-driven assignments
  yhsozfhbxy <= 3_2_0 ps;
  mevt <= '1';
end kvybabwo;

entity kxrub is
  port (nhfwun : inout real; sf : out real);
end kxrub;

architecture akg of kxrub is
  
begin
  
end akg;



-- Seed after: 13602840436073690140,5472058987609252853
