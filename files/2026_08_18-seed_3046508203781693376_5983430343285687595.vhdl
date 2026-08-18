-- Seed: 3046508203781693376,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity hetdtv is
  port (eti : buffer std_logic);
end hetdtv;

architecture egsuyeoi of hetdtv is
  
begin
  -- Multi-driven assignments
  eti <= 'X';
  eti <= eti;
  eti <= eti;
  eti <= 'U';
end egsuyeoi;

entity cchzgldal is
  port (twua : in integer; avlbw : linkage string(1 downto 4));
end cchzgldal;

library ieee;
use ieee.std_logic_1164.all;

architecture ksyamgcamm of cchzgldal is
  signal suci : std_logic;
begin
  vlabzleyaf : entity work.hetdtv
    port map (eti => suci);
  jvybh : entity work.hetdtv
    port map (eti => suci);
  sytyvmzvmj : entity work.hetdtv
    port map (eti => suci);
  dwvyazkrhu : entity work.hetdtv
    port map (eti => suci);
  
  -- Multi-driven assignments
  suci <= suci;
  suci <= suci;
  suci <= suci;
  suci <= 'X';
end ksyamgcamm;

library ieee;
use ieee.std_logic_1164.all;

entity ull is
  port (ffrurmk : inout bit; tmtb : out std_logic; gvufa : inout time_vector(0 to 3));
end ull;

library ieee;
use ieee.std_logic_1164.all;

architecture tiiuk of ull is
  signal bwkkl : std_logic;
begin
  j : entity work.hetdtv
    port map (eti => tmtb);
  bvcimatxws : entity work.hetdtv
    port map (eti => tmtb);
  hoovjkhbio : entity work.hetdtv
    port map (eti => tmtb);
  d : entity work.hetdtv
    port map (eti => bwkkl);
  
  -- Single-driven assignments
  ffrurmk <= ffrurmk;
  
  -- Multi-driven assignments
  bwkkl <= '-';
  tmtb <= '0';
end tiiuk;



-- Seed after: 5172860162775310474,5983430343285687595
