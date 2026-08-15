-- Seed: 8491514163918742943,2230106469645304029

entity xl is
  port (iypfcchl : buffer integer; jlajooca : buffer real);
end xl;

architecture dlrjz of xl is
  
begin
  -- Single-driven assignments
  iypfcchl <= 2224;
  jlajooca <= jlajooca;
end dlrjz;

entity waykqyxy is
  port (lzdvbqrdb : in integer_vector(3 downto 1); uzhh : buffer integer);
end waykqyxy;

architecture krkwisn of waykqyxy is
  signal zteudzne : real;
  signal c : integer;
  signal iqjemzf : real;
  signal kiznrp : real;
  signal mxhnn : integer;
  signal d : real;
  signal os : integer;
begin
  n : entity work.xl
    port map (iypfcchl => os, jlajooca => d);
  bw : entity work.xl
    port map (iypfcchl => mxhnn, jlajooca => kiznrp);
  kz : entity work.xl
    port map (iypfcchl => uzhh, jlajooca => iqjemzf);
  l : entity work.xl
    port map (iypfcchl => c, jlajooca => zteudzne);
end krkwisn;

entity rzgdlt is
  port (edgt : out real);
end rzgdlt;

architecture tmviooku of rzgdlt is
  signal atqo : integer;
  signal vu : real;
  signal ia : integer;
  signal e : real;
  signal yniqnvjldk : integer;
  signal dpc : integer;
  signal abtmbxe : integer_vector(3 downto 1);
begin
  lgavmuevhw : entity work.waykqyxy
    port map (lzdvbqrdb => abtmbxe, uzhh => dpc);
  jtig : entity work.xl
    port map (iypfcchl => yniqnvjldk, jlajooca => e);
  fwbnr : entity work.xl
    port map (iypfcchl => ia, jlajooca => vu);
  g : entity work.xl
    port map (iypfcchl => atqo, jlajooca => edgt);
  
  -- Single-driven assignments
  abtmbxe <= (8#5#, 4_0_0, 110);
end tmviooku;

entity tuptuycnvl is
  port (t : out character);
end tuptuycnvl;

architecture eo of tuptuycnvl is
  
begin
  
end eo;



-- Seed after: 1636906365663034730,2230106469645304029
