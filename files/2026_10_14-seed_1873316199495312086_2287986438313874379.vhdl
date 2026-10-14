-- Seed: 1873316199495312086,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity yo is
  port (qsnabwfh : in bit; iavzgfnqjd : inout time; ncdm : out real; pjrnftx : out std_logic);
end yo;

architecture zvuprhk of yo is
  
begin
  -- Single-driven assignments
  iavzgfnqjd <= iavzgfnqjd;
  ncdm <= ncdm;
  
  -- Multi-driven assignments
  pjrnftx <= '1';
  pjrnftx <= pjrnftx;
end zvuprhk;

library ieee;
use ieee.std_logic_1164.all;

entity cou is
  port (vdbvgtefha : in time_vector(0 downto 2); bjcf : inout std_logic; sbqnevgfog : out boolean);
end cou;

library ieee;
use ieee.std_logic_1164.all;

architecture asizn of cou is
  signal xoexk : real;
  signal qc : time;
  signal zvdohkuvqp : bit;
  signal utol : std_logic;
  signal pzwwn : real;
  signal tqxgnwcrtc : time;
  signal jvkks : bit;
  signal g : std_logic;
  signal ctnyrog : real;
  signal urwllwjps : time;
  signal mnsyvznwyx : std_logic;
  signal kyujsrqo : real;
  signal lh : time;
  signal gaul : bit;
begin
  otbg : entity work.yo
    port map (qsnabwfh => gaul, iavzgfnqjd => lh, ncdm => kyujsrqo, pjrnftx => mnsyvznwyx);
  rguwrkmd : entity work.yo
    port map (qsnabwfh => gaul, iavzgfnqjd => urwllwjps, ncdm => ctnyrog, pjrnftx => g);
  xbuhhaydt : entity work.yo
    port map (qsnabwfh => jvkks, iavzgfnqjd => tqxgnwcrtc, ncdm => pzwwn, pjrnftx => utol);
  lpwgjspz : entity work.yo
    port map (qsnabwfh => zvdohkuvqp, iavzgfnqjd => qc, ncdm => xoexk, pjrnftx => bjcf);
  
  -- Single-driven assignments
  sbqnevgfog <= TRUE;
  zvdohkuvqp <= gaul;
  
  -- Multi-driven assignments
  mnsyvznwyx <= bjcf;
  utol <= bjcf;
end asizn;

library ieee;
use ieee.std_logic_1164.all;

entity wi is
  port (vslgju : in std_logic; qp : in bit_vector(2 downto 1); gchxzj : out character);
end wi;

library ieee;
use ieee.std_logic_1164.all;

architecture epqr of wi is
  signal aqwnkvh : std_logic;
  signal eotffvncrh : real;
  signal i : time;
  signal yzssqg : bit;
begin
  jervulpfec : entity work.yo
    port map (qsnabwfh => yzssqg, iavzgfnqjd => i, ncdm => eotffvncrh, pjrnftx => aqwnkvh);
  
  -- Single-driven assignments
  gchxzj <= gchxzj;
  yzssqg <= '1';
  
  -- Multi-driven assignments
  aqwnkvh <= 'Z';
  aqwnkvh <= vslgju;
end epqr;

library ieee;
use ieee.std_logic_1164.all;

entity rzlf is
  port (fdf : buffer character; bht : inout std_logic);
end rzlf;

architecture ixrvmo of rzlf is
  signal tdhlite : boolean;
  signal svefzpj : real;
  signal ivf : time;
  signal cpehuyo : bit;
  signal zrwnohacxh : boolean;
  signal fjzscuxcgz : time_vector(0 downto 2);
begin
  vrzztuwkk : entity work.cou
    port map (vdbvgtefha => fjzscuxcgz, bjcf => bht, sbqnevgfog => zrwnohacxh);
  al : entity work.yo
    port map (qsnabwfh => cpehuyo, iavzgfnqjd => ivf, ncdm => svefzpj, pjrnftx => bht);
  rii : entity work.cou
    port map (vdbvgtefha => fjzscuxcgz, bjcf => bht, sbqnevgfog => tdhlite);
  
  -- Single-driven assignments
  fdf <= 's';
  fjzscuxcgz <= (others => 0 ns);
  
  -- Multi-driven assignments
  bht <= '0';
  bht <= bht;
end ixrvmo;



-- Seed after: 9512732376147467604,2287986438313874379
