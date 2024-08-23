--
-- ================ synthsizer configuration =================== 		
-- altera vhdl_input_version vhdl_2008
-- ============================================================= 
library ieee;
use ieee.std_logic_1164.all;

entity dec_8b10b_n is
generic (
	FLIP_10B		: boolean := true;
	CHANNEL_WIDTH	: natural := 1;
    g_BYTES : positive := 1--;
);
port (
    --i_data      : in    std_logic_vector(g_BYTES*10-1 downto 0);

    --o_data      : out   std_logic_vector(g_BYTES*8-1 downto 0);
    --o_datak     : out   std_logic_vector(g_BYTES-1 downto 0);

    --o_err       : out   std_logic;
	-- Avalon-ST [in10b]
	asi_in10b_data		: in  std_logic_vector(9 downto 0);
	asi_in10b_valid		: in  std_logic;
	asi_in10b_channel 	: in  std_logic_vector(CHANNEL_WIDTH-1 downto 0);
	asi_in10b_error		: in  std_logic; 
	
	-- Avalon-ST [out8b]
	aso_out8b_data		: out std_logic_vector(8 downto 0);
	aso_out8b_valid		: out std_logic;
	aso_out8b_channel 	: out std_logic_vector(CHANNEL_WIDTH-1 downto 0);
	aso_out8b_error		: out std_logic_vector(2 downto 0);
		-- error mapping: disperr(0) & err(0) & av_error_dly(1);

    i_reset_n   : in    std_logic;
    i_clk       : in    std_logic--;
);
end entity;

architecture arch of dec_8b10b_n is

    signal data10,data10_flip : std_logic_vector(9 downto 0);
    signal data : std_logic_vector(7 downto 0);
    signal datak : std_logic_vector(0 downto 0);
    signal disp : std_logic_vector(g_BYTES downto 0);
    signal disperr, err : std_logic_vector(g_BYTES-1 downto 0);
    signal disperr_q, err_q : std_logic_vector(g_BYTES-1 downto 0);
	
	signal din_10b				: std_logic_vector(9 downto 0);
	signal din_10b_vld			: std_logic_vector(1 downto 0);
	signal av_error_dly			: std_logic_vector(1 downto 0);
	
	
	signal asi_i0_data					: std_logic_vector(9 downto 0);
	signal asi_i0_valid, asi_i0_error 	: std_logic;
	signal asi_i0_channel 				: std_logic_vector(CHANNEL_WIDTH-1 downto 0);
	
	signal aso_o0_data					: std_logic_vector(8 downto 0);
	signal aso_o0_valid					: std_logic;
	signal aso_o0_channel				: std_logic_vector(CHANNEL_WIDTH-1 downto 0);
	signal aso_o0_error					: std_logic_vector(2 downto 0);
	

begin

	asi_i0_data		<= asi_in10b_data;
	asi_i0_valid	<= asi_in10b_valid;
	asi_i0_error	<= asi_in10b_error;
	asi_i0_channel	<= asi_in10b_channel;
	
	aso_out8b_data		<= aso_o0_data;
	aso_out8b_valid		<= aso_o0_valid;
	aso_out8b_channel	<= aso_o0_channel;
	aso_out8b_error		<= aso_o0_error;
	
	proc_avst : process(i_clk,i_reset_n,asi_i0_valid) 
	begin
		if  i_reset_n = '0'  then
			aso_o0_data		<= (others=>'0');
			aso_o0_valid	<= '0';
			aso_o0_channel	<= (others=>'0');
			aso_o0_error	<= (others=>'0');
			din_10b_vld(0)	<= '0';
			av_error_dly(0)	<= '0';
			disp(1)			<= '0';
		elsif rising_edge(i_clk) then
			din_10b_vld(1)	<= din_10b_vld(0);
			av_error_dly(0)	<= asi_i0_error;
			av_error_dly(1)	<= av_error_dly(0);
			aso_o0_channel	<= asi_i0_channel; -- channel has no latency comp.
			aso_o0_error	<= disperr(0) & err(0) & av_error_dly(1);
			disp(1)			<= disp(0);
			if asi_i0_valid then
			-- input reg, w/ data 1 cyc latency
				din_10b			<= asi_i0_data;
				din_10b_vld(0)	<= '1';
			else
				din_10b_vld(0)	<= '0';
			end if;
			-- output reg, 
			aso_o0_data		<= datak & data;
			aso_o0_valid	<= din_10b_vld(1);
		end if;
	end process proc_avst;
	
	
--	gen_flip_all_lane	: for i in 0 to g_BYTES-1 generate 
--	begin 
--		gen_flip_byte	: for n in 0 to 9 generate 
--		begin 
--            data10_flip(i*10+n) <= din_10b(i*10+9-n) when FLIP_10B = true else din_10b(i*10+n);
--        end generate gen_flip_byte;
--	end generate gen_flip_all_lane;


	gen_flip_byte	: for n in 0 to 9 generate 
	begin 
		data10_flip(n) <= din_10b(9-n) when FLIP_10B = true else din_10b(n);
	end generate gen_flip_byte;

	
	e_enc_8b10b : entity work.dec_8b10b
        port map (
            i_data => data10_flip(9 downto 0),
            i_disp => disp(1),
            o_data(7 downto 0) => data(7 downto 0),
            o_data(8) => datak(0),
            o_disp => disp(0),
            o_disperr => disperr(0),
            o_err => err(0)--,
        );


    --o_err <= work.util.or_reduce(disperr_q or err_q);

--    process(i_clk, i_reset_n)
--    begin
--    if ( i_reset_n /= '1' ) then
--        data10 <= (others => '0');
--        o_data <= (others => '0');
--        o_datak <= (others => '0');
--        disp(0) <= '0';
--        --
--    elsif rising_edge(i_clk) then
--        data10 <= data10_flip;
--        o_data <= data;
--        o_datak <= datak;
--        disp(0) <= disp(g_BYTES);
--        disperr_q <= disperr;
--        err_q <= err;
--        --
--    end if;
--    end process;

end architecture;
