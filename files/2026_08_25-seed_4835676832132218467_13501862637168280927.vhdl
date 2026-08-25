-- Seed: 4835676832132218467,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity msanln is
  port ( glbuzxyl : linkage std_logic_vector(2 downto 4)
  ; hjt : linkage time
  ; agco : linkage bit_vector(2 downto 3)
  ; laug : linkage integer_vector(4 to 1)
  );
end msanln;

architecture ao of msanln is
  
begin
  
end ao;

entity deu is
  port (lu : linkage integer; liyo : inout boolean);
end deu;

library ieee;
use ieee.std_logic_1164.all;

architecture dqf of deu is
  signal syabsshux : integer_vector(4 to 1);
  signal e : bit_vector(2 downto 3);
  signal ya : time;
  signal gpxq : integer_vector(4 to 1);
  signal ljk : bit_vector(2 downto 3);
  signal uticnlpoq : time;
  signal atsd : std_logic_vector(2 downto 4);
begin
  ibuhfawiy : entity work.msanln
    port map (glbuzxyl => atsd, hjt => uticnlpoq, agco => ljk, laug => gpxq);
  yyeehrawc : entity work.msanln
    port map (glbuzxyl => atsd, hjt => ya, agco => e, laug => syabsshux);
  
  -- Single-driven assignments
  liyo <= TRUE;
  
  -- Multi-driven assignments
  atsd <= "";
  atsd <= atsd;
end dqf;

entity yemuxpafjh is
  port (lwcwlb : linkage time; oa : in time_vector(3 to 2));
end yemuxpafjh;

architecture ucbhrejpi of yemuxpafjh is
  
begin
  
end ucbhrejpi;

library ieee;
use ieee.std_logic_1164.all;

entity eh is
  port (vcxwbgmu : buffer std_logic_vector(3 downto 3));
end eh;

library ieee;
use ieee.std_logic_1164.all;

architecture ggtw of eh is
  signal j : integer_vector(4 to 1);
  signal llfo : bit_vector(2 downto 3);
  signal oohxtms : time;
  signal brustc : integer_vector(4 to 1);
  signal g : bit_vector(2 downto 3);
  signal muoyfr : time;
  signal lmxxc : std_logic_vector(2 downto 4);
begin
  m : entity work.msanln
    port map (glbuzxyl => lmxxc, hjt => muoyfr, agco => g, laug => brustc);
  ost : entity work.msanln
    port map (glbuzxyl => lmxxc, hjt => oohxtms, agco => llfo, laug => j);
end ggtw;



-- Seed after: 119250317719167471,13501862637168280927
