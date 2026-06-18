-- Seed: 5123268156889079669,8118127366649987907

entity qnpugq is
  port (gcbbhm : out bit_vector(2 downto 1); rchfrpglw : linkage time; k : in time);
end qnpugq;

architecture rohdqu of qnpugq is
  
begin
  -- Single-driven assignments
  gcbbhm <= ('1', '0');
end rohdqu;

entity wvo is
  port (pgvuv : out severity_level);
end wvo;

architecture zmwxkvg of wvo is
  signal lupbgbnhf : time;
  signal xhteogprv : time;
  signal pbmfb : bit_vector(2 downto 1);
  signal fhiy : bit_vector(2 downto 1);
  signal wrtdtmjd : time;
  signal s : bit_vector(2 downto 1);
  signal lnnhg : time;
  signal fh : time;
  signal btuibg : bit_vector(2 downto 1);
begin
  wy : entity work.qnpugq
    port map (gcbbhm => btuibg, rchfrpglw => fh, k => lnnhg);
  li : entity work.qnpugq
    port map (gcbbhm => s, rchfrpglw => wrtdtmjd, k => fh);
  vlvdbk : entity work.qnpugq
    port map (gcbbhm => fhiy, rchfrpglw => lnnhg, k => fh);
  ttzlwkxbhl : entity work.qnpugq
    port map (gcbbhm => pbmfb, rchfrpglw => xhteogprv, k => lupbgbnhf);
  
  -- Single-driven assignments
  lupbgbnhf <= 3 us;
  pgvuv <= FAILURE;
end zmwxkvg;



-- Seed after: 11186854656508401981,8118127366649987907
